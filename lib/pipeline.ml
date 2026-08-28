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
    deps_only = false }

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
  | None -> Format.eprintf "typr: could not parse %s@." path ; None
  | Some (prog, extras) -> Some { path ; prog ; extras ; defs = R_deps.defs_of_program prog }

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
let process_r_file ctx f =
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
    try Driver.treat_def ctx past
    with e ->
      warn "definition"
        (Driver.toplevel_name past |> Option.value ~default:"<statement>") e ;
      ctx) ctx (order_defs items)

(* ===== Native side ===== *)

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

(* Type-checks the C sources and returns a lookup from a native symbol name to
   the type inferred for it, together with the printing environment to carry
   over to the R phase (it holds the aliases declared by the [.ty] files, so
   that both phases print types the same way). *)
let run_native opts (pkg : Pkg.t) entry_points =
  Mlsem.System.Config.infer_overload := true ;
  setup_include_dirs opts.include_dirs ;
  if pkg.c_files = [] then ((fun _ -> None), R_c_typing.Defs.parsed_types_penv)
  else
    let run () =
      let pasts = R_c_typing.Runner.parse_files opts.native pkg.c_files in
      (* With no entry point NativeSem would keep nothing reachable; a package
         whose R side we could not read is still worth typing in full. *)
      let entry_points = if entry_points = [] then None else Some entry_points in
      R_c_typing.Runner.run_on_pasts opts.native pasts ?entry_points
        R_c_typing.Runner.StrMap.empty R_c_typing.Defs.initial_env
    in
    let (idenv, env), penv =
      Mlsem.Types.PEnv.sequential_handler R_c_typing.Defs.parsed_types_penv run ()
    in
    let lookup name =
      R_c_typing.Runner.find_existing_binding name idenv env
      |> Option.map (fun (_, tys) ->
          Mlsem.Types.(TyScheme.get tys |> snd |> GTy.ub))
    in
    (lookup, penv)

(* ===== Pipeline ===== *)

(* What TypR resolved, without running either checker. *)
let report_deps (pkg : Pkg.t) files =
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
  Format.printf "@.C files: %d@." (List.length pkg.c_files)

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

  if opts.deps_only then report_deps pkg files
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
          process_r_file ctx f)
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
        process_r_file ctx { f with prog = Link.rewrite_native_calls known f.prog })
        ctx files
      |> ignore) ()
    |> ignore
  end
