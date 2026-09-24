(** The TypR pipeline.

    Parses a package once, resolves the dependencies between its R and native
    functions, and drives the two type-checkers in that order: the native side
    first (an R function can call into C, not the other way round), then the R
    side with the inferred native types injected into its environment. *)

open Lang

module StrMap = Map.Make(String)
module StrSet = R_deps.StrSet
module TyScheme = Mlsem.Types.TyScheme
module GTy = Mlsem.Types.GTy

type options = {
  native : R_c_typing.Runner.cmd_options ;
  (* R files holding the signatures of the functions the package builds upon
     (base R, and the packages it imports). Processed before the package. *)
  prelude : string list ;
  (* Extra directories to search for C headers. *)
  include_dirs : string list ;
  (* Wall-clock limit on type-checking a single function. Applies to both
     sides: it overrides [native.timeout] when set. *)
  timeout : float option ;
  (* Give the [dyn] type to the R names nothing binds, instead of reporting
     them. Off by default. Only the R side has the choice: NativeSem already
     types the C identifiers it cannot resolve this way. *)
  gradual : bool ;
  (* Print the dependency information instead of type-checking. *)
  deps_only : bool ;
  (* Where to write the machine-readable account of the run, if anywhere. *)
  report : string option ;
}

let default_native_options : R_c_typing.Runner.cmd_options = {
  cst = false ; past = false ; ast = false ; mlsem = false ; typing = true ;
  debug = false ; filter = None ; timeout = None ;
  fallback_c_signature = false ; call_graph = None ; log_times = false ;
}

let default_options =
  { native = default_native_options ; prelude = [] ; include_dirs = [] ;
    timeout = None ; gradual = false ; deps_only = false ; report = None }

(* NativeSem has a per-function timeout of its own, for when it is used as a
   standalone CLI. Driven from here the policy comes from [Timeout.guard]
   instead, so that both languages are bounded by the same code; disable the
   internal one so the two cannot both fire. *)
let native_options opts = { opts.native with timeout = None }

(* ===== Reporting ===== *)

(* The report handle, plus the counters the [summary] record is made of. *)
type rep = { r : Report.t ; stats : (string, int) Hashtbl.t }

let emit rep kind fields = Report.emit rep.r kind fields

let bump rep key =
  Hashtbl.replace rep.stats key
    (1 + Option.value ~default:0 (Hashtbl.find_opt rep.stats key))

let opt_s = function None -> Report.N | Some s -> Report.S s

(* Line and column of a top-level item, from the position both parsers carry. *)
let pos_fields pos =
  let open Mlsem.Common in
  if pos = Position.dummy then [ ("line", Report.N) ; ("col", Report.N) ]
  else
    let p = Position.start_of_position pos in
    [ ("line", Report.I (Position.line p)) ; ("col", Report.I (Position.column p)) ]

(* NativeSem delivers a unit's outcome through [Runner.on_outcome], global
   like its normalization hook and bracketed the same way. *)
let with_outcome_hook handler f =
  let saved = !R_c_typing.Runner.on_outcome in
  R_c_typing.Runner.on_outcome := handler ;
  Fun.protect ~finally:(fun () -> R_c_typing.Runner.on_outcome := saved) f

(* ===== Ordering ===== *)

(* Topological order of [nodes] w.r.t. [deps] (the nodes a node must follow).
   Cycles -- mutually recursive definitions, which R allows freely -- are
   broken by keeping the input order inside them, which is what the checkers
   would have seen anyway. *)
let topo_sort nodes deps =
  let visited = Hashtbl.create 64 and out = ref [] in
  let rec visit n =
    match Hashtbl.find_opt visited n with
    | Some _ -> ()
    | None ->
      Hashtbl.add visited n `Visiting ;
      List.iter visit (deps n) ;
      Hashtbl.replace visited n `Done ;
      out := n :: !out
  in
  List.iter visit nodes ;
  List.rev !out

(* ===== R side ===== *)

type r_file = {
  path : string ;
  prog : PAst.t ;
  extras : Tree_sitter_r.CST.extra list ;
  defs : R_deps.def list ;
}

(* [role] is ["package"] or ["prelude"]; either way the file gets a record. *)
let parse_r rep role path =
  let file ~parsed ?error n_defs =
    emit rep "file"
      [ ("side", Report.S "r") ; ("role", Report.S role) ; ("path", Report.S path) ;
        ("parsed", Report.B parsed) ; ("parse_error", opt_s error) ; ("n_defs", Report.I n_defs) ] in
  match Driver.parse path with
  | None ->
    Format.printf "typr: could not parse %s@.@." path ;
    file ~parsed:false ~error:"parse error" 0 ; None
  | Some (prog, extras) ->
    let defs = R_deps.defs_of_program prog in
    file ~parsed:true (List.length defs) ;
    Some { path ; prog ; extras ; defs }
  | exception e ->
    Format.printf "typr: could not parse %s (%s)@.@." path (Printexc.to_string e) ;
    file ~parsed:false ~error:(Printexc.to_string e) 0 ; None

(* Order the R files so that a file comes after the files defining the names it
   uses. The dependency graph is computed between *definitions*; it is then
   condensed onto files, because Rsem attaches the [##] annotations of a file to
   the definitions of that same file, which is only sound if a file is fed to
   it as a whole. *)
let order_r_files files =
  let defining =
    files |> List.fold_left (fun acc f ->
      f.defs |> List.fold_left (fun acc (d : R_deps.def) ->
        match d.name with
        | Some n -> StrMap.add n f.path acc
        | None -> acc) acc) StrMap.empty
  in
  let deps_of f =
    f.defs
    |> List.concat_map (fun (d : R_deps.def) -> StrSet.elements d.uses)
    |> List.filter_map (fun n -> StrMap.find_opt n defining)
    |> List.filter (fun p -> p <> f.path)
    |> List.sort_uniq String.compare
  in
  let by_path = files |> List.map (fun f -> (f.path, f)) |> StrMap.of_list in
  let order = topo_sort (List.map (fun f -> f.path) files)
      (fun p -> deps_of (StrMap.find p by_path)) in
  List.map (fun p -> StrMap.find p by_path) order

(* A top-level definition binding a function literal. R evaluates top-level
   statements in order but resolves the free variables of a function body only
   when it is called, so such a definition may be moved; anything else is
   evaluated eagerly and stays where it is. *)
let is_function_def (_, e) =
  match e with
  | PAst.Binop (("<-" | "="), ((_, PAst.Id _), (_, PAst.Function _)))
  | PAst.Binop ("->", ((_, PAst.Function _), (_, PAst.Id _))) -> true
  | _ -> false

(* Order the top-level items of a file so that each is checked after what it
   needs. R evaluates top-level statements in order, but resolves the free
   variables of a function body only when the function is called, so the two
   kinds of item get different rules:

   - a function definition may follow anything it uses, wherever that is
     written -- this is what lets a file define its functions in any order;
   - a statement is evaluated on the spot, so it may only rely on what precedes
     it, and statements keep their relative order.

   Rsem checks definitions one at a time in the order it is given them, so this
   is what decides whether a use resolves. Mutually recursive definitions are a
   cycle: no order satisfies them, and one of the two is still reported as
   unbound. *)
let order_defs items =
  let arr = Array.of_list items in
  let n = Array.length arr in
  let def i = (snd arr.(i) : R_deps.def) in
  let names = List.init n (fun i -> (def i).name) |> List.filter_map Fun.id in
  if List.length names <> List.length (List.sort_uniq String.compare names)
  then
    (* A name is defined twice in this file: which definition a use refers to
       depends on the order, so leave it alone. *)
    items
  else
    let defining =
      List.init n (fun i -> Option.map (fun nm -> (nm, i)) (def i).name)
      |> List.filter_map Fun.id |> StrMap.of_list
    in
    (* The statement each item has to stay after, so that statements keep their
       relative order. *)
    let prev_stmt = Array.make n (-1) in
    let last = ref (-1) in
    for i = 0 to n - 1 do
      prev_stmt.(i) <- !last ;
      if not (is_function_def (fst arr.(i))) then last := i
    done ;
    let deps i =
      let is_fun = is_function_def (fst arr.(i)) in
      let used =
        StrSet.elements (def i).uses
        |> List.filter_map (fun nm -> StrMap.find_opt nm defining)
        |> List.filter (fun j -> j <> i && (is_fun || j < i))
      in
      if is_fun || prev_stmt.(i) < 0 then used else prev_stmt.(i) :: used
    in
    topo_sort (List.init n Fun.id) deps |> List.map (fun i -> arr.(i))

(* Type-check one R file. This is [Driver.process], with the error recovery a
   package scan needs: Rsem treats every [##] comment as a type annotation and
   fails on the ones it cannot parse, but real packages use [##] for ordinary
   comments (roxygen and usethis both emit them), and one bad definition must
   not take the rest of the package down with it.

   Every definition yields one [r_def] record. Its status comes from what Rsem
   *did* -- the guard says whether it ran out of time, [Driver.find] whether
   the name ended up typed -- and only an error's title, position and
   description come from the block Rsem printed, which [Report.capture] scopes
   to this one definition. [role] tells the prelude from the package;
   [defined] and [native_syms] classify an unbound name. *)
let process_r_file ?timeout rep ~role ~defined ~native_syms ctx f =
  let module R = Report in
  let warn what name exn =
    Format.printf "typr: skipping %s %s (%s)@.@." what name (Printexc.to_string exn) in
  let extra_name (`Comment (_, (_, str))) =
    let str = String.trim str in
    if String.length str <= 40 then str else String.sub str 0 40 ^ "..." in
  (* The annotations declared by the previous files precede everything here. *)
  let ctx = { ctx with Driver.lannots =
    ctx.Driver.lannots |> List.map (fun a -> { a with Driver.loffset = min_int }) } in
  let ctx =
    List.fold_left (fun ctx extra ->
      try Driver.treat_extra f.prog ctx extra
      with e ->
        warn "annotation" (extra_name extra) e ;
        emit rep "annotation_error"
          [ ("file", R.S f.path) ; ("comment", R.S (extra_name extra)) ;
            ("exn", R.S (Printexc.to_string e)) ] ;
        ctx) ctx f.extras
  in
  let items = List.combine f.prog (R_deps.defs_of_program f.prog) in
  let package = role = "package" in
  List.fold_left (fun ctx (past, (def : R_deps.def)) ->
    let name = Driver.toplevel_name past |> Option.value ~default:"<statement>" in
    (* Rsem prints an anonymous statement as [_]. *)
    let printed = Option.value ~default:"_" def.name in
    (* Checked against a [#|] signature, rather than inferred. *)
    let annotated =
      match Option.bind def.name (fun n -> StrMap.find_opt n ctx.Driver.idenv) with
      | Some v -> Mlsem.Common.VarMap.mem v ctx.Driver.senv
      | None -> false
    in
    let started = Unix.gettimeofday () in
    (* [capture] sits outside the guard, so that the guard's own report of a
       timeout lands in this definition's block. On timeout the definition
       keeps no type, so the ones that use it report an unbound variable; the
       rest of the package is still checked. *)
    let (outcome, block) =
      R.capture (fun () ->
        match Timeout.guard' timeout ~name ~unchanged:ctx (fun () -> Driver.treat_def ctx past) with
        | (ctx', false) -> `Done ctx'
        | (ctx', true) -> `Timeout ctx'
        | exception e -> warn "definition" name e ; `Skipped e)
    in
    let elapsed = Unix.gettimeofday () -. started in
    let typed_as n ctx' =
      match Driver.find ctx' n with
      | Some tys ->
        let ty = TyScheme.get tys |> snd |> GTy.ub in
        Some (R.one_line TyScheme.pp_short tys, R.degenerate ty)
      | None -> None
    in
    let ctx', status, ty, err, skipped =
      match outcome with
      | `Timeout ctx' -> ctx', "timeout", None, None, None
      | `Skipped e -> ctx, "skipped", None, None, Some (Printexc.to_string e)
      | `Done ctx' ->
        (match R.untypeable_of_block ~name:printed block with
         | Some u -> ctx', "untypeable", None, Some u, None
         | None ->
           (match def.name with
            | None -> ctx', "typed", None, None, None
            | Some n ->
              (match typed_as n ctx' with
               | Some ty -> ctx', "typed", Some ty, None, None
               (* No error block, yet the name got no type: a hoisted-name
                  failure ([name: msg]) or something new. Not a success. *)
               | None -> ctx', "unknown", None, None, None)))
    in
    let error_name = Option.bind err R.unbound_name in
    let unbound_kind =
      match error_name with
      | None -> None
      | Some n when StrSet.mem n defined -> Some "package"
      | Some n when StrSet.mem n native_syms -> Some "native"
      | Some _ -> Some "prelude"
    in
    let loc_fields =
      match Option.bind err (fun (u : R.untypeable) -> u.loc) with
      | Some (l, c1, c2) ->
        [ ("error_line", R.I l) ; ("error_col_start", R.I c1) ; ("error_col_end", R.I c2) ]
      | None -> [ ("error_line", R.N) ; ("error_col_start", R.N) ; ("error_col_end", R.N) ]
    in
    emit rep "r_def"
      ([ ("name", R.S name) ; ("anonymous", R.B (def.name = None)) ; ("file", R.S f.path) ;
         ("role", R.S role) ] @ pos_fields (fst past) @
       [ ("annotated", R.B annotated) ; ("status", R.S status) ;
         ("type", opt_s (Option.map fst ty)) ;
         ("degenerate", R.B (match ty with Some (_, d) -> d | None -> false)) ;
         ("error_title", opt_s (Option.map (fun (u : R.untypeable) -> u.title) err)) ]
       @ loc_fields @
       [ ("error_name", opt_s error_name) ; ("unbound_kind", opt_s unbound_kind) ;
         ("error_detail", opt_s (Option.bind err (fun (u : R.untypeable) -> u.descr))) ;
         ("elapsed_sec", R.F elapsed) ; ("n_uses", R.I (StrSet.cardinal def.uses)) ;
         ("natives", R.L (List.map (fun (n : R_deps.native_call) -> n.symbol) def.natives)) ;
         ("skipped_exn", opt_s skipped) ]) ;
    if package then begin
      bump rep (if def.name = None then "r_n_stmts" else "r_n_defs") ;
      if def.name <> None then begin
        bump rep ("r_n_" ^ status) ;
        Option.iter (fun k -> bump rep ("r_n_unbound_" ^ k)) unbound_kind ;
        (match ty with Some (_, true) -> bump rep "r_n_empty_ret" | _ -> ())
      end
    end ;
    ctx')
    ctx (order_defs items)

(* ===== Native side ===== *)

(* The two languages disagree on what "no value" is: a C function returning
   [void] yields mlsem's unit, an R expression yields NULL. [Driver.setup]
   installs the R answer globally, so the native phase has to put the default
   back for its own duration -- otherwise a void C function is typed [null]. *)
let with_void_ty ty f =
  let saved = !Mlsem.Lang.Config.void_ty in
  Mlsem.Lang.Config.void_ty := ty ;
  Fun.protect ~finally:(fun () -> Mlsem.Lang.Config.void_ty := saved) f

(* Same story for mlsem's substitution-normalization hook. Both checkers set
   it: Rsem in [Driver.setup] (which [bin/main.ml] calls first, so it wins the
   whole process), NativeSem at module initialisation. They used to agree; they
   no longer do -- NativeSem's rule is per variable, because bare primitives
   (CHARSXPs) exist only at the C level and rstt's whole-component rule erases
   their content. So the native phase must run under NativeSem's hook, and the
   R phase under Rsem's, or the native types TypR links differ from what
   NativeSem itself infers. *)
let with_native_hooks f =
  let saved = !Mlsem.System.Config.subst_normalization_fun in
  Mlsem.System.Config.subst_normalization_fun := R_c_typing.Runner.subst_normalization ;
  Fun.protect
    ~finally:(fun () -> Mlsem.System.Config.subst_normalization_fun := saved) f

(* Where the C preprocessor looks for the headers a package includes. *)
let setup_include_dirs include_dirs =
  let env_dirs =
    match Sys.getenv_opt "C_INCLUDE_PATH" with
    | None | Some "" -> []
    | Some s -> String.split_on_char ':' s |> List.filter (fun x -> x <> "")
  in
  R_c_typing.Parser.set_include_dirs
    (include_dirs @ env_dirs @ R_c_typing.Utils.detect_gcc_include_dirs ()
     @ R_c_typing.Parser.default_include_dirs)

(* Type-check the C sources, and return a lookup from a native symbol name to
   the type inferred for it, together with the printing environment to carry
   over to the R phase (it holds the aliases declared by the [.ty] files, so
   that both phases print types the same way).

   The scheduling is TypR's, not NativeSem's: the call graph is built here (see
   {!C_deps}), so a native function is ordered and bounded exactly like an R
   one. NativeSem is called one top-level unit at a time, through
   [Runner.infer_def]. *)
let run_native opts rep (pkg : Pkg.t) entry_points =
  let module R = Report in
  let module Rn = R_c_typing.Runner in
  Mlsem.System.Config.infer_overload := true ;
  setup_include_dirs opts.include_dirs ;
  if pkg.c_files = [] then ((fun _ -> None), R_c_typing.Defs.parsed_types_penv)
  else
    let module Runner = R_c_typing.Runner in
    let module PAst = R_c_typing.PAst in
    let module NStrMap = Runner.StrMap in
    let opts_native = native_options opts in
    let visible _ = true in
    let conventions = StrMap.of_list entry_points in
    (* The outcomes NativeSem reports for the unit being checked: reset before
       each [infer_def], folded into one [native_def] record after it. *)
    let outcomes = ref [] in
    let on_outcome ~name:_ ~elapsed o = outcomes := (elapsed, o) :: !outcomes in
    let header = function
      | `Default -> "default" | `SimpleC -> "simple_c" | `DotC -> "dot_c" | `Define -> "define" in
    let pp_tys tys = R.one_line TyScheme.pp_short tys in
    let degenerate tys = TyScheme.get tys |> snd |> GTy.ub |> R.degenerate in
    (* A function that fails and is then bound at its C signature reports
       twice -- the failure, then [Fallback]. Folding the pair is what tells a
       substituted signature from a real inference. [timed_out] is TypR's own
       guard firing: NativeSem's internal timer is off under TypR, so its
       [Timeout] outcome never comes. *)
    let native_def ~kind ~file ~(past : PAst.top_level_unit) ~calls ~timed_out ~wall =
      let name = PAst.top_level_unit_name past in
      let outs = List.rev !outcomes in
      let typed = List.find_map (function
          | (e, Rn.Typed { header = h ; tys ; _ }) -> Some (e, h, tys) | _ -> None) outs in
      let fallback = List.find_map (function
          | (e, Rn.Fallback tys) -> Some (e, tys) | _ -> None) outs in
      let failure = List.find_map (function
          | (e, Rn.Untypeable { title ; descr }) -> Some (e, "untypeable", title, descr)
          | (e, Rn.Timeout s) -> Some (e, "timeout", Printf.sprintf "exceeded %g s" s, None)
          | (e, Rn.Internal m) -> Some (e, "internal", m, None)
          | _ -> None) outs in
      let status, hdr, ty, err, elapsed =
        if timed_out then ("timeout", None, None, None, wall)
        else match typed, fallback, failure with
          | Some (e, h, tys), _, _ -> ("typed", Some (header h), Some tys, None, e)
          | None, Some (e, tys), f ->
            ("fallback", None, Some tys, Option.map (fun (_, _, t, d) -> (t, d)) f, e)
          | None, None, Some (e, st, t, d) -> (st, None, None, Some (t, d), e)
          | None, None, None -> ("skipped", None, None, None, wall)
      in
      emit rep "native_def"
        ([ ("name", R.S name) ; ("kind", R.S kind) ; ("file", R.S file) ] @ pos_fields (fst past) @
         [ ("convention", opt_s (Option.map R_c_typing.Package.calling_convention_to_string
                                   (StrMap.find_opt name conventions))) ;
           ("is_entry_point", R.B (StrMap.mem name conventions)) ;
           ("is_declaration", R.B (C_deps.is_declaration past)) ;
           ("calls", R.L calls) ; ("status", R.S status) ; ("header_kind", opt_s hdr) ;
           ("type", opt_s (Option.map pp_tys ty)) ;
           ("degenerate", R.B (match ty with Some t -> degenerate t | None -> false)) ;
           ("error_title", opt_s (Option.map fst err)) ;
           ("error_detail", opt_s (Option.bind err snd)) ;
           ("elapsed_sec", R.F elapsed) ]) ;
      if kind = "function" then bump rep ("native_n_" ^ status)
    in
    let run () =
      let t0 = Unix.gettimeofday () in
      let pasts = Runner.parse_files opts_native pkg.c_files in
      emit rep "phase"
        [ ("name", R.S "c_parsing") ; ("elapsed_sec", R.F (Unix.gettimeofday () -. t0)) ;
          ("count", R.I (List.length pasts)) ] ;
      (* [infer_def]'s [Include] arm recurses over every item of a system
         header. Those never reach the hook, so they are counted, not listed. *)
      let n_include_items = ref 0 in
      pasts |> List.iter (fun (file, past) ->
        past |> List.iter (function
          | _, PAst.Include items -> n_include_items := !n_include_items + List.length items
          | _ -> ()) ;
        emit rep "file"
          [ ("side", R.S "c") ; ("role", R.S "package") ; ("path", R.S file) ;
            ("parsed", R.B true) ; ("parse_error", R.N) ;
            ("n_defs", R.I (List.length (List.filter C_deps.is_fundef past))) ]) ;
      emit rep "phase"
        [ ("name", R.S "include_items") ; ("elapsed_sec", R.N) ; ("count", R.I !n_include_items) ] ;

      (* Every type declaration must be known before any global is typed: a
         global declared in a file that does not see the struct body would
         otherwise be registered with an empty record, and the later, complete
         declaration cannot upgrade it. *)
      let rec collect_type_decls decl item =
        match item with
        | _, PAst.TypeDecl (name, ty) ->
          R_c_typing.Ast.DeclMap.add name (PAst.resolve_ctype decl ty) decl
        | _, PAst.Include items -> List.fold_left collect_type_decls decl items
        | _ -> decl
      in
      let decl =
        List.fold_left
          (fun decl (_, past) -> List.fold_left collect_type_decls decl past)
          R_c_typing.Ast.DeclMap.empty pasts
      in

      (* A global defined in several translation units is a different variable
         in each; so is a [static] one. Both get a per-file identifier
         environment rather than the shared one. *)
      let conflicted =
        List.fold_left (fun acc (file, past) ->
          List.fold_left (fun acc item ->
            match item with
            | _, PAst.GlobalVar (PAst.Definition, name, _) ->
              StrMap.update name
                (fun fs -> Some (StrSet.add file (Option.value ~default:StrSet.empty fs)))
                acc
            | _ -> acc) acc past) StrMap.empty pasts
        |> StrMap.filter (fun _ fs -> StrSet.cardinal fs > 1)
      in

      (* Everything that is not a function definition, in source order. *)
      let idenv, env, decl, file_idenvs =
        List.fold_left (fun acc (file, past) ->
          List.fold_left (fun (idenv, env, decl, file_idenvs) item ->
            let internal =
              match item with
              | _, PAst.GlobalVar (PAst.Static, _, _) -> true
              | _, PAst.GlobalVar (PAst.Definition, _, _) ->
                StrMap.mem (PAst.top_level_unit_name item) conflicted
              | _ -> false
            in
            let kind =
              match item with
              | _, PAst.GlobalVar _ -> Some "global"
              | _, PAst.Define _ -> Some "define"
              | _, PAst.TypeDecl _ -> Some "typedecl"
              | _ -> None
            in
            outcomes := [] ;
            let t0 = Unix.gettimeofday () in
            let result =
              match item with
              | _, PAst.Fundef _ -> (idenv, env, decl, file_idenvs)
              | _ when internal ->
                let own =
                  StrMap.find_opt file file_idenvs |> Option.value ~default:NStrMap.empty in
                let own, env, decl =
                  Runner.infer_def ~internal_scope:file ~force_internal_global:true
                    visible opts_native (own, env, decl) item
                in
                (idenv, env, decl, StrMap.add file own file_idenvs)
              | _ ->
                let idenv, env, decl =
                  Runner.infer_def ~internal_scope:file visible opts_native
                    (idenv, env, decl) item
                in
                (idenv, env, decl, file_idenvs)
            in
            kind |> Option.iter (fun kind ->
              native_def ~kind ~file ~past:item ~calls:[] ~timed_out:false
                ~wall:(Unix.gettimeofday () -. t0)) ;
            result)
            acc past)
          (NStrMap.empty, R_c_typing.Defs.initial_env, decl, StrMap.empty) pasts
      in

      (* The function definitions, callees first, restricted to what the entry
         points reach. *)
      let fun_names = C_deps.fun_names pasts in
      let defs =
        C_deps.fundefs ~fun_names pasts
        |> C_deps.reachable ~roots:(List.map fst entry_points)
      in
      let by_name = defs |> List.map (fun (d : C_deps.fundef) -> (d.name, d)) |> StrMap.of_list in
      let ordered =
        topo_sort (List.map (fun (d : C_deps.fundef) -> d.name) defs)
          (fun n -> (StrMap.find n by_name).calls)
        |> List.map (fun n -> StrMap.find n by_name)
      in
      Hashtbl.replace rep.stats "native_n_functions" (List.length ordered) ;
      let t0 = Unix.gettimeofday () in
      let idenv, env, _ =
        List.fold_left (fun (idenv, env, decl) (d : C_deps.fundef) ->
          let own =
            StrMap.find_opt d.file file_idenvs |> Option.value ~default:NStrMap.empty in
          let unchanged =
            (NStrMap.union (fun _ local _global -> Some local) own idenv, env, decl) in
          outcomes := [] ;
          let t1 = Unix.gettimeofday () in
          let (idenv', env, decl), timed_out =
            Timeout.guard' opts.timeout ~name:d.name ~unchanged (fun () ->
              Runner.infer_def ~internal_scope:d.file
                ~convention:(StrMap.find_opt d.name conventions)
                visible opts_native unchanged d.past)
          in
          native_def ~kind:"function" ~file:d.file ~past:d.past ~calls:d.calls ~timed_out
            ~wall:(Unix.gettimeofday () -. t1) ;
          let idenv =
            match NStrMap.find_opt d.name idenv' with
            | Some v -> NStrMap.add d.name v idenv
            | None -> idenv
          in
          (idenv, env, decl))
          (idenv, env, decl) ordered
      in
      emit rep "phase"
        [ ("name", R.S "native_functions") ; ("elapsed_sec", R.F (Unix.gettimeofday () -. t0)) ;
          ("count", R.I (List.length ordered)) ] ;
      (idenv, env)
    in
    let (idenv, env), penv =
      with_void_ty Mlsem.Types.Ty.unit (fun () -> with_native_hooks (fun () ->
        with_outcome_hook on_outcome (fun () ->
          Mlsem.Types.PEnv.sequential_handler R_c_typing.Defs.parsed_types_penv run ())))
    in
    let lookup name =
      Runner.find_existing_binding name idenv env
      |> Option.map (fun (_, tys) ->
          Mlsem.Types.(TyScheme.get tys |> snd |> GTy.ub))
    in
    (lookup, penv)

(* ===== Pipeline ===== *)

(* What TypR resolved, without running either checker. *)
let report_deps opts (pkg : Pkg.t) entry_points files =
  let defined =
    files |> List.concat_map (fun f -> f.defs)
    |> List.filter_map (fun (d : R_deps.def) -> d.name)
    |> StrSet.of_list
  in
  Format.printf "R files (dependency order):@." ;
  files |> List.iter (fun f ->
    Format.printf "  %s@." (Filename.basename f.path) ;
    f.defs |> List.iter (fun (d : R_deps.def) ->
      let name = Option.value ~default:"<statement>" d.name in
      (* Only the dependencies on the package's own definitions: everything
         else comes from base R or an imported package. *)
      let r_deps =
        StrSet.inter d.uses defined |> StrSet.remove name |> StrSet.elements in
      let natives = d.natives |> List.map (fun (n : R_deps.native_call) ->
        Printf.sprintf ".%s(%s)"
          (R_c_typing.Package.calling_convention_to_string n.convention)
          n.symbol) in
      match r_deps @ natives with
      | [] -> ()
      | deps -> Format.printf "    %s -> %s@." name (String.concat ", " deps))) ;
  Format.printf "@." ;
  (* The native side is resolved here too, by the same means: parse, collect
     what each function references, order callees first. *)
  Format.printf "Native functions (dependency order):@." ;
  let pasts = R_c_typing.Runner.parse_files opts.native pkg.c_files in
  let fun_names = C_deps.fun_names pasts in
  let defs =
    C_deps.fundefs ~fun_names pasts |> C_deps.reachable ~roots:(List.map fst entry_points) in
  let by_name = defs |> List.map (fun (d : C_deps.fundef) -> (d.name, d)) |> StrMap.of_list in
  topo_sort (List.map (fun (d : C_deps.fundef) -> d.name) defs)
    (fun n -> (StrMap.find n by_name).calls)
  |> List.iter (fun n ->
      let d = StrMap.find n by_name in
      match d.calls with
      | [] -> Format.printf "  %s (%s)@." d.name (Filename.basename d.file)
      | calls ->
        Format.printf "  %s (%s) -> %s@." d.name (Filename.basename d.file)
          (String.concat ", " calls))

let run opts root =
  Driver.gradual := opts.gradual ;
  let module R = Report in
  let rep = { r = R.create opts.report ; stats = Hashtbl.create 32 } in
  let started = Unix.gettimeofday () in
  let body () =
    let pkg = Pkg.scan root in
    (* Provenance comes from the image (see the Dockerfile): absent locally. *)
    let prov k = opt_s (Sys.getenv_opt k) in
    emit rep "run"
      [ ("schema", R.I 1) ; ("package", R.S (Filename.basename root)) ; ("root", R.S root) ;
        ("mode", R.S (if opts.gradual then "gradual" else "strict")) ;
        ("gradual", R.B opts.gradual) ;
        ("timeout", (match opts.timeout with Some t -> R.F t | None -> R.N)) ;
        ("prelude", R.L opts.prelude) ;
        ("fallback_c_signature", R.B opts.native.fallback_c_signature) ;
        ("log_times", R.B opts.native.log_times) ; ("prefix", R.S pkg.prefix) ;
        ("n_r_files", R.I (List.length pkg.r_files)) ;
        ("n_c_files", R.I (List.length pkg.c_files)) ;
        ("load_ty_sec", R.F !R_c_typing.Defs.ty_load_time) ;
        ("prov_typr", prov "TYPR_SHA") ; ("prov_rsem", prov "RSEM_SHA") ;
        ("prov_nativesem", prov "NATIVESEM_SHA") ; ("prov_rstt", prov "RSTT_SHA") ;
        ("prov_pinned", prov "TYPR_PINNED") ] ;
    let t0 = Unix.gettimeofday () in
    let files = List.filter_map (parse_r rep "package") pkg.r_files in
    let files = order_r_files files in
    emit rep "phase"
      [ ("name", R.S "r_parsing") ; ("elapsed_sec", R.F (Unix.gettimeofday () -. t0)) ;
        ("count", R.I (List.length files)) ] ;

    (* Native entry points, taken from the parsed R code rather than from a regex
       over the sources: every symbol reached by a [.Call]/[.C]/... anywhere in
       the package, under the name the C side gives it. *)
    let natives =
      files |> List.concat_map (fun f -> f.defs) |> R_deps.natives_of_defs in
    let entry_points =
      StrMap.bindings natives
      |> List.map (fun (r_name, conv) -> (Pkg.native_symbol ~prefix:pkg.prefix r_name, conv)) in
    natives |> StrMap.iter (fun r_name conv ->
      emit rep "entry_point"
        [ ("r_name", R.S r_name) ; ("symbol", R.S (Pkg.native_symbol ~prefix:pkg.prefix r_name)) ;
          ("convention", R.S (R_c_typing.Package.calling_convention_to_string conv)) ]) ;
    Hashtbl.replace rep.stats "n_ep" (StrMap.cardinal natives) ;
    (* For classifying an unbound R name: the package's own definitions, and
       the native symbols it reaches. Everything else is the prelude's. *)
    let defined =
      files |> List.concat_map (fun f -> f.defs)
      |> List.filter_map (fun (d : R_deps.def) -> d.name) |> StrSet.of_list in
    let native_syms = StrMap.bindings natives |> List.map fst |> StrSet.of_list in

    if opts.deps_only then report_deps opts pkg entry_points files
    else begin
      Format.printf "@.@{<bold>===== Native code =====@}@.@." ;
      let native_ty, penv = run_native opts rep pkg entry_points in

      Format.printf "@.@{<bold>===== R code =====@}@.@." ;
      (* Everything below prints types, which needs the printing environment the
         native phase produced (it holds the [.ty] aliases). *)
      Mlsem.Types.PEnv.sequential_handler penv (fun () ->
        let ctx =
          List.fold_left (fun ctx f ->
            Format.printf "@.@{<bold>===== prelude %s =====@}@." f.path ;
            process_r_file ?timeout:opts.timeout rep ~role:"prelude" ~defined ~native_syms ctx f)
            Driver.initial_ctx (List.filter_map (parse_r rep "prelude") opts.prelude)
        in
        (* Bind each native symbol under its R-visible name, with its C type
           adapted to an R calling convention. *)
        let ctx, known =
          StrMap.fold (fun r_name _conv (ctx, known) ->
            let c_name = Pkg.native_symbol ~prefix:pkg.prefix r_name in
            let native = native_ty c_name in
            let result =
              match native with
              | None -> Error Link.No_native_type
              | Some ty -> Link.r_type_of_native' ty in
            let pp_ty ty = R.one_line Mlsem.Types.Ty.pp ty in
            emit rep "link"
              [ ("r_name", R.S r_name) ; ("symbol", R.S c_name) ;
                ("native_typed", R.B (native <> None)) ;
                ("native_type", opt_s (Option.map pp_ty native)) ;
                ("convertible", R.B (Result.is_ok result)) ;
                ("reject_reason",
                 (match result with Error r -> R.S (Link.string_of_reject r) | Ok _ -> R.N)) ;
                ("bound", R.B (Result.is_ok result)) ;
                ("r_type", (match result with Ok ty -> R.S (pp_ty ty) | Error _ -> R.N)) ;
                ("degenerate", R.B (match result with Ok ty -> R.degenerate ty | Error _ -> false)) ] ;
            match result with
            | Error _ ->
              Format.printf "%s: no native type available@.@." r_name ;
              (ctx, known)
            | Ok ty ->
              bump rep "n_ep_linked" ;
              let gty = Mlsem.Types.GTy.mk ty in
              Format.printf "%s: @[%a@]@.@." r_name Mlsem.Types.GTy.pp gty ;
              (Driver.bind ctx r_name gty, StrSet.add r_name known))
            natives (ctx, StrSet.empty)
        in
        let known n = StrSet.mem n known in
        List.fold_left (fun ctx f ->
          Format.printf "@.@{<bold>===== %s =====@}@." f.path ;
          process_r_file ?timeout:opts.timeout rep ~role:"package" ~defined ~native_syms ctx
            { f with prog = Link.rewrite_native_calls known f.prog })
          ctx files
        |> ignore) ()
      |> ignore
    end ;
    let stats =
      Hashtbl.fold (fun k v acc -> (k, R.I v) :: acc) rep.stats [] |> List.sort compare in
    emit rep "summary"
      (stats @ [ ("elapsed_sec", R.F (Unix.gettimeofday () -. started)) ; ("complete", R.B true) ])
  in
  (* A run killed from outside has no [summary]; one that dies from inside
     gets a [crash] record before the exception propagates. *)
  (match body () with
   | () -> ()
   | exception e ->
     emit rep "crash"
       [ ("exn", R.S (Printexc.to_string e)) ; ("backtrace", R.S (Printexc.get_backtrace ())) ] ;
     R.close rep.r ; raise e) ;
  R.close rep.r
