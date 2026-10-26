(* R values: their R source and their type.

   With [--cases], prints one JSON line per value with its R source, for
   [rval_check.R] to evaluate and describe. With [--compare CASES DESCS],
   checks that R's descriptions type the values as OCaml's do. *)

open Mlsem.Types
open Typr.Rval

let s x = Vec (Chr, [S x])
let d x = Vec (Dbl, [D x])

let values = [
  Null ;
  Vec (Lgl, [B true]) ; Vec (Lgl, [NA]) ; Vec (Lgl, [B false ; NA ; B true]) ;
  Vec (Int, [I 3]) ; Vec (Int, [I (-3)]) ; Vec (Int, [NA]) ; Vec (Int, []) ;
  Vec (Int, [I 1 ; I 2 ; NA ; I 4 ; I 5]) ;
  d 42. ; d (-0.5) ; d 1e15 ; d 1e300 ; d Float.nan ; d Float.infinity ;
  d Float.neg_infinity ; Vec (Dbl, [NA]) ; Vec (Dbl, [D 1. ; D 2.5]) ;
  Vec (Clx, [C (1., -2.)]) ; Vec (Clx, [NA]) ;
  s "a" ; s "" ; s "NA" ; s "say \"hi\"\\n\n\t" ; s "\xc3\xa9t\xc3\xa9" ;
  Vec (Chr, [NA]) ; Vec (Chr, [S "a" ; S "b"]) ;
  Vec (Raw, [I 255]) ; Vec (Raw, [I 0 ; I 1]) ; Vec (Raw, []) ;
  List [] ; List [None, d 1.] ; List [Some "a", d 1. ; None, s "x"] ;
  List [Some "a b", List [Some "c", Null]] ;
  Env ; Sym "x" ; Lang "f(x, 1)" ; Fun (d 1.) ;
  With { v = Vec (Int, [I 1]) ; cls = Some ["factor"] ; attrs = ["levels", s "a"] } ;
  With { v = Vec (Dbl, [D 1. ; D 2.]) ; cls = None ;
         attrs = ["names", Vec (Chr, [S "a" ; S "b"])] } ;
  With { v = Vec (Int, [I 1 ; I 2 ; I 3 ; I 4]) ; cls = None ;
         attrs = ["dim", Vec (Int, [I 2 ; I 2])] } ;
  With { v = List [Some "x", d 1.] ; cls = Some ["foo" ; "bar"] ; attrs = [] } ;
  With { v = Env ; cls = Some ["R6"] ; attrs = [] } ;
]

let print_values () =
  values |> List.iter (fun v ->
    let vt = to_valty v in
    Printf.printf "%s\n  : %s%s\n" (to_r v)
      (Typr.Report.one_line Ty.pp vt.ty) (if vt.exact then "" else "  (inexact)"))

let print_cases () =
  values |> List.iter (fun v ->
    print_endline (Yojson.Safe.to_string (`Assoc ["src", `String (to_r v)])))

let compare_descs descs =
  let descs = In_channel.with_open_text descs In_channel.input_lines in
  if List.length descs <> List.length values then
    Printf.printf "%d values, %d descriptions\n" (List.length values) (List.length descs)
  else begin
    let bad = List.combine values descs |> List.filter (fun (v, desc) ->
      let ours = to_valty v and theirs = Typr.Valty.of_json (Yojson.Safe.from_string desc) in
      if Ty.equiv ours.ty theirs.ty && ours.exact = theirs.exact then false
      else begin
        Printf.printf "%s\n  OCaml: %s\n  R:     %s\n" (to_r v)
          (Yojson.Safe.to_string (describe v)) desc ;
        true
      end) in
    Printf.printf "%d values, %d disagreements\n" (List.length values) (List.length bad)
  end

let () =
  Lang.Driver.setup () ;
  let run () =
    match Sys.argv with
    | [| _ ; "--cases" |] -> print_cases ()
    | [| _ ; "--compare" ; _ ; descs |] -> compare_descs descs
    | _ -> print_values ()
  in
  PEnv.sequential_handler PEnv.empty run () |> ignore
