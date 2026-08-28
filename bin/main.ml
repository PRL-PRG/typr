let usage = "typr [--deps] [--debug] [--timeout SECONDS] <package-directory>"

let deps_only = ref false
let debug = ref false
let timeout = ref None
let include_dirs = ref []
let prelude = ref []
let root = ref None

let speclist =
  [ ("--deps", Arg.Set deps_only,
     "Only report the dependencies between R and native functions") ;
    ("--debug", Arg.Set debug, "Print intermediate information") ;
    ("--timeout", Arg.Float (fun f -> timeout := Some f),
     "SECONDS  Per-function timeout for native inference") ;
    ("-I", Arg.String (fun d -> include_dirs := !include_dirs @ [d]),
     "DIR  Additional directory to search for C headers") ;
    ("--prelude", Arg.String (fun f -> prelude := !prelude @ [f]),
     "FILE  R file of signatures to load before the package (repeatable)") ]

let () =
  Printexc.record_backtrace true ;
  (* NativeSem configures the printers when its library is loaded; this adds
     what the R side needs on top (in particular [void_ty]). *)
  Lang.Driver.setup () ;
  Arg.parse speclist (fun a -> root := Some a) usage ;
  match !root with
  | None -> prerr_endline usage ; exit 1
  | Some root when not (Sys.file_exists root && Sys.is_directory root) ->
    Printf.eprintf "typr: not a package directory: %s\n" root ; exit 1
  | Some root ->
    let native =
      { Typr.Pipeline.default_native_options with
        debug = !debug ; timeout = !timeout }
    in
    Typr.Pipeline.run
      { native ; prelude = !prelude ; include_dirs = !include_dirs ;
        deps_only = !deps_only } root
