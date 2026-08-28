(** Layout of an R package: which sources belong to the R side, which to the
    native side, and how R names native symbols. *)

(* The C side is exactly what NativeSem already collects (headers first, and
   the transitive closure of [src/], since packages vendor helper libraries in
   subdirectories). *)
let get_c_files = R_c_typing.Package.get_c_files

let r_extensions = [ ".R" ; ".r" ; ".S" ; ".s" ; ".q" ]

let get_r_files path =
  let r_dir = Filename.concat path "R" in
  if not (Sys.file_exists r_dir && Sys.is_directory r_dir) then []
  else
    Sys.readdir r_dir |> Array.to_list
    |> List.filter (fun f -> List.exists (Filename.check_suffix f) r_extensions)
    (* R itself collates alphabetically unless the DESCRIPTION says otherwise;
       the dependency ordering computed later refines this. *)
    |> List.sort String.compare
    |> List.map (Filename.concat r_dir)

(* [useDynLib(lib, .registration = TRUE, .fixes = "PREFIX_")] prefixes the R
   variables bound to the registered routines, while the native symbols keep
   their bare name. Returns "" when there is no such declaration. *)
let native_prefix path =
  let namespace = Filename.concat path "NAMESPACE" in
  if not (Sys.file_exists namespace) then ""
  else
    try
      let content =
        In_channel.with_open_text namespace In_channel.input_lines
        |> String.concat "\n"
      in
      let re = Str.regexp "useDynLib.*\\.fixes[ \t]*=[ \t]*\"\\([^\"]+\\)\"" in
      ignore (Str.search_forward re content 0) ;
      Str.matched_group 1 content
    with _ -> ""

(* The native symbol an R-visible name refers to. *)
let native_symbol ~prefix name =
  if prefix <> "" && String.starts_with ~prefix name
  then String.sub name (String.length prefix) (String.length name - String.length prefix)
  else name

type t = {
  root : string ;
  r_files : string list ;
  c_files : string list ;
  prefix : string ;      (* [.fixes] prefix of the R-visible symbol names *)
}

let scan root =
  if not (Sys.file_exists root && Sys.is_directory root) then
    failwith (Printf.sprintf "Not a package directory: %s" root) ;
  { root ; r_files = get_r_files root ; c_files = get_c_files root ;
    prefix = native_prefix root }
