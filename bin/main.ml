let usage =
  "typr [--deps] [--gradual] [--debug] [--timeout SECONDS] [--prelude FILE]\n\
  \      [--report FILE] [--fallback-c-signature] [--log-times] [--call-graph FILE]\n\
  \      [-I DIR] <package-directory>"

let deps_only = ref false
let gradual = ref false
let debug = ref false
let timeout = ref None
let include_dirs = ref []
let prelude = ref []
let report = ref None
let fallback = ref false
let log_times = ref false
let call_graph = ref None
let root = ref None

let speclist =
  [ ("--deps", Arg.Set deps_only,
     "Only report the dependencies between R and native functions") ;
    ("--debug", Arg.Set debug, "Print intermediate information") ;
    ("--gradual", Arg.Set gradual,
     "Give the dyn type to the R names nothing binds, instead of reporting them") ;
    ("--timeout", Arg.Float (fun f -> timeout := Some f),
     "SECONDS  Give up on a function whose type-checking takes longer") ;
    ("-I", Arg.String (fun d -> include_dirs := !include_dirs @ [d]),
     "DIR  Additional directory to search for C headers") ;
    ("--prelude", Arg.String (fun f -> prelude := !prelude @ [f]),
     "FILE  R file of signatures to load before the package (repeatable)") ;
    ("--report", Arg.String (fun f -> report := Some f),
     "FILE  Write a machine-readable account of the run (one JSON record per line)") ;
    ("--fallback-c-signature", Arg.Set fallback,
     "Bind a native function that fails to type at its declared C signature") ;
    ("--log-times", Arg.Set log_times,
     "Print NativeSem's per-function timing lines (the report has them regardless)") ;
    ("--call-graph", Arg.String (fun f -> call_graph := Some f),
     "FILE  Write the native call graph in Graphviz format") ]

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
        debug = !debug ; fallback_c_signature = !fallback ; log_times = !log_times ;
        call_graph = !call_graph } in
    Typr.Pipeline.run
      { native ; prelude = !prelude ; include_dirs = !include_dirs ;
        timeout = !timeout ; gradual = !gradual ; deps_only = !deps_only ;
        report = !report } root
