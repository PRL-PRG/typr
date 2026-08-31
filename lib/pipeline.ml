(** The TypR pipeline.

    Parses a package once, resolves the dependencies between its R and native
    functions, and drives the two type-checkers in that order: the native side
    first (an R function can call into C, not the other way round), then the R
    side with the inferred native types injected into its environment. *)

open Lang

module StrMap = Map.Make(String)
module StrSet = R_deps.StrSet

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
  (* Print the dependency information instead of type-checking. *)
  deps_only : bool ;
}

let default_native_options : R_c_typing.Runner.cmd_options = {
  cst = false ; past = false ; ast = false ; mlsem = false ; typing = true ;
  debug = false ; filter = None ; timeout = None ;
  fallback_c_signature = false ; call_graph = None ; log_times = false ;
}

let default_options =
  { native = default_native_options ; prelude = [] ; include_dirs = [] ;
    timeout = None ; deps_only = false }

(* NativeSem has a per-function timeout of its own, for when it is used as a
   standalone CLI. Driven from here the policy comes from [Timeout.guard]
   instead, so that both languages are bounded by the same code; disable the
   internal one so the two cannot both fire. *)
let native_options opts = { opts.native with timeout = None }

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

let parse_r path =
  match Driver.parse path with
  | None -> Format.printf "typr: could not parse %s@.@." path ; None
  | Some (prog, extras) -> Some { path ; prog ; extras ; defs = R_deps.defs_of_program prog }
  | exception e ->
    Format.printf "typr: could not parse %s (%s)@.@." path (Printexc.to_string e) ;
    None

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
   not take the rest of the package down with it. *)
let process_r_file ?timeout ctx f =
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
      with e -> warn "annotation" (extra_name extra) e ; ctx) ctx f.extras
  in
  let items = List.combine f.prog (R_deps.defs_of_program f.prog) in
  List.fold_left (fun ctx (past, _) ->
    let name = Driver.toplevel_name past |> Option.value ~default:"<statement>" in
    (* On timeout the definition keeps no type, so the ones that use it report
       an unbound variable; the rest of the package is still checked. *)
    try Timeout.guard timeout ~name ~unchanged:ctx (fun () -> Driver.treat_def ctx past)
    with e -> warn "definition" name e ; ctx)
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
let run_native opts (pkg : Pkg.t) entry_points =
  Mlsem.System.Config.infer_overload := true ;
  setup_include_dirs opts.include_dirs ;
  if pkg.c_files = [] then ((fun _ -> None), R_c_typing.Defs.parsed_types_penv)
  else
    let module Runner = R_c_typing.Runner in
    let module PAst = R_c_typing.PAst in
    let module NStrMap = Runner.StrMap in
    let opts_native = native_options opts in
    let visible _ = true in
    let run () =
      let pasts = Runner.parse_files opts_native pkg.c_files in

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
              (idenv, env, decl, file_idenvs))
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
      let conventions = StrMap.of_list entry_points in
      let idenv, env, _ =
        List.fold_left (fun (idenv, env, decl) (d : C_deps.fundef) ->
          let own =
            StrMap.find_opt d.file file_idenvs |> Option.value ~default:NStrMap.empty in
          let unchanged =
            (NStrMap.union (fun _ local _global -> Some local) own idenv, env, decl) in
          let idenv', env, decl =
            Timeout.guard opts.timeout ~name:d.name ~unchanged (fun () ->
              Runner.infer_def ~internal_scope:d.file
                ~convention:(StrMap.find_opt d.name conventions)
                visible opts_native unchanged d.past)
          in
          let idenv =
            match NStrMap.find_opt d.name idenv' with
            | Some v -> NStrMap.add d.name v idenv
            | None -> idenv
          in
          (idenv, env, decl))
          (idenv, env, decl) ordered
      in
      (idenv, env)
    in
    let (idenv, env), penv =
      with_void_ty Mlsem.Types.Ty.unit (fun () ->
        Mlsem.Types.PEnv.sequential_handler R_c_typing.Defs.parsed_types_penv run ())
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
  let pkg = Pkg.scan root in
  let files = List.filter_map parse_r pkg.r_files in
  let files = order_r_files files in

  (* Native entry points, taken from the parsed R code rather than from a regex
     over the sources: every symbol reached by a [.Call]/[.C]/... anywhere in
     the package, under the name the C side gives it. *)
  let natives =
    files |> List.concat_map (fun f -> f.defs) |> R_deps.natives_of_defs in
  let entry_points =
    StrMap.bindings natives
    |> List.map (fun (r_name, conv) -> (Pkg.native_symbol ~prefix:pkg.prefix r_name, conv)) in

  if opts.deps_only then report_deps opts pkg entry_points files
  else begin
    Format.printf "@.@{<bold>===== Native code =====@}@.@." ;
    let native_ty, penv = run_native opts pkg entry_points in

    Format.printf "@.@{<bold>===== R code =====@}@.@." ;
    (* Everything below prints types, which needs the printing environment the
       native phase produced (it holds the [.ty] aliases). *)
    Mlsem.Types.PEnv.sequential_handler penv (fun () ->
      (* Bind each native symbol under its R-visible name, with its C type
         adapted to an R calling convention. *)
      let ctx =
        List.fold_left (fun ctx f ->
          Format.printf "@.@{<bold>===== prelude %s =====@}@." f.path ;
          process_r_file ?timeout:opts.timeout ctx f)
          Driver.initial_ctx (List.filter_map parse_r opts.prelude)
      in
      let ctx, known =
        StrMap.fold (fun r_name _conv (ctx, known) ->
          let c_name = Pkg.native_symbol ~prefix:pkg.prefix r_name in
          match Option.bind (native_ty c_name) Link.r_type_of_native with
          | None ->
            Format.printf "%s: no native type available@.@." r_name ;
            (ctx, known)
          | Some ty ->
            let gty = Mlsem.Types.GTy.mk ty in
            Format.printf "%s: @[%a@]@.@." r_name Mlsem.Types.GTy.pp gty ;
            (Driver.bind ctx r_name gty, StrSet.add r_name known))
          natives (ctx, StrSet.empty)
      in
      let known n = StrSet.mem n known in
      List.fold_left (fun ctx f ->
        Format.printf "@.@{<bold>===== %s =====@}@." f.path ;
        process_r_file ?timeout:opts.timeout ctx
          { f with prog = Link.rewrite_native_calls known f.prog })
        ctx files
      |> ignore) ()
    |> ignore
  end
