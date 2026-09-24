(** The machine-readable account of a run: one JSON record per line (JSONL),
    written as the run goes and flushed at every record, so that a run killed
    half-way still leaves everything up to that point readable. The consumer is
    r-typing's [parse_output.R], through [jsonlite::stream_in].

    Records are flat -- scalar fields only, list-valued fields [;]-joined -- so
    that [stream_in(..., simplifyDataFrame = TRUE)] yields a data frame with no
    unnesting. Absent values are emitted as [null] rather than omitted, so the
    column set is stable across the records of one kind. *)

open Mlsem.Types

(* [None]: reporting is disabled and every [emit] is a no-op. *)
type t = out_channel option

let create = function
  | None -> None
  | Some path -> Some (open_out path)

let close t = Option.iter close_out t

type v =
  | S of string
  | I of int
  | F of float
  | B of bool
  | N                   (* null *)
  | L of string list    (* one [;]-joined string *)

(* Yojson writes a non-finite float as [NaN] / [Infinity], which is not JSON
   and which jsonlite rejects: those become [null]. *)
let json_of_v : v -> Yojson.Safe.t = function
  | S s -> `String s
  | I i -> `Int i
  | F f -> if Float.is_finite f then `Float f else `Null
  | B b -> `Bool b
  | N -> `Null
  | L l -> `String (String.concat ";" l)

(* [emit t kind fields] writes [{"k": kind, ...}] on one line. Yojson escapes
   the newlines a multi-line type signature contains, which is what keeps the
   one-record-per-line invariant. *)
let emit t kind fields =
  t |> Option.iter (fun oc ->
    let record =
      `Assoc (("k", `String kind) :: List.map (fun (k, v) -> (k, json_of_v v)) fields) in
    output_string oc (Yojson.Safe.to_string record) ;
    output_char oc '\n' ;
    flush oc)

(* ===== Types ===== *)

(* A type printed on one line. [Format.asprintf] wraps at 78 columns, and a
   wrapped signature is an awkward value for a table cell or a CSV field. *)
let one_line pp x =
  (* An [h] box turns every break hint into a space, as NativeSem's own
     printing does; a forced newline still gets through, hence the pass. *)
  let s = Format.asprintf "@[<h>%a@]" pp x in
  String.split_on_char '\n' s |> List.map String.trim
  |> List.filter (fun l -> l <> "") |> String.concat " "

(* A function type that can never return: every arrow has an empty codomain.
   Inference concluding this about a definition is worth a column of its own.
   Same unwrapping of the R attributes as [Link.r_type_of_native']. *)
let degenerate ty =
  let content = if Ty.leq ty Rstt.Attr.any then Rstt.Attr.proj_content ty else ty in
  not (Ty.is_empty content) && Ty.leq content Arrow.any
  && Arrow.dnf content
     |> List.for_all (List.for_all (fun (_dom, codom) -> Ty.is_empty codom))

(* ===== Output capture ===== *)

(* [capture f] runs [f] with everything it prints on the standard formatter
   collected, re-emits that output verbatim, and returns it with the result.

   Rsem reports a definition's outcome only by printing it -- the structured
   error is thrown away -- so this is how the R side gets a per-definition text
   to parse. It is scoped, not stream, scraping: one [Driver.treat_def] call is
   exactly one printed block, which is the delimitation the old dashboard's
   line scanner never had. Exceptions propagate, the output already re-emitted.

   Every out-function is redirected, not just [out_string]: the stock
   [out_newline] and [out_spaces] of a formatter write to the underlying
   channel directly and would bypass the buffer. *)
let capture f =
  let fmt = Format.std_formatter in
  Format.pp_print_flush fmt () ;
  let saved = Format.pp_get_formatter_out_functions fmt () in
  let buf = Buffer.create 1024 in
  Format.pp_set_formatter_out_functions fmt
    { out_string = Buffer.add_substring buf ;
      out_flush = (fun () -> ()) ;
      out_newline = (fun () -> Buffer.add_char buf '\n') ;
      out_spaces = (fun n -> Buffer.add_string buf (String.make n ' ')) ;
      out_indent = (fun n -> Buffer.add_string buf (String.make n ' ')) } ;
  let restore () =
    Format.pp_print_flush fmt () ;
    Format.pp_set_formatter_out_functions fmt saved ;
    saved.out_string (Buffer.contents buf) 0 (Buffer.length buf) ;
    saved.out_flush ()
  in
  match f () with
  | result -> restore () ; (result, Buffer.contents buf)
  | exception e -> restore () ; raise e

(* ===== The R side's printed block ===== *)

(* What Rsem prints for a definition it could not type:
     name: Untypeable: <title>
     [Line N, characters A-B  |  File "f", line N, characters A-B]
     [descr, possibly several lines]
   The header is deliberately matched with the name, because a success line
   ([name: <type>]) and a hoisted-name failure ([name: msg], driver.ml:185)
   have the same shape and neither is an "Untypeable:" line. *)
type untypeable = { title : string ; loc : (int * int * int) option ; descr : string option }

let untypeable_of_block ~name block =
  let lines =
    String.split_on_char '\n' block |> List.map String.trim
    |> List.filter (fun l -> l <> "") in
  let prefix = name ^ ": Untypeable: " in
  let scan_loc l =
    try Some (Scanf.sscanf l "Line %d, characters %d-%d" (fun a b c -> (a, b, c)))
    with _ ->
      try Some (Scanf.sscanf l "File %S, line %d, characters %d-%d" (fun _ a b c -> (a, b, c)))
      with _ -> None
  in
  let rec find = function
    | [] -> None
    | l :: rest when String.starts_with ~prefix l ->
      let title = String.sub l (String.length prefix) (String.length l - String.length prefix) in
      let loc, rest =
        match rest with
        | l :: rest' -> (match scan_loc l with Some loc -> (Some loc, rest') | None -> (None, rest))
        | [] -> (None, [])
      in
      let descr = match rest with [] -> None | ls -> Some (String.concat "\n" ls) in
      Some { title ; loc ; descr }
    | _ :: rest -> find rest
  in
  find lines

(* The name an [unbound variable] error is about: its descr is [name: <id>]. *)
let unbound_name (u : untypeable) =
  match u.title, u.descr with
  | "unbound variable", Some d when String.starts_with ~prefix:"name: " d ->
    Some (String.trim (String.sub d 6 (String.length d - 6)))
  | _ -> None
