(* Type-directed generation: a few values drawn for each type, with a fixed
   seed. [Gen.sample] only returns values whose type is a subtype of the
   requested one; the test checks it again, and shows the values.

   With [--cases] and [--compare CASES DESCS], as [rval_test]: checks that
   the values, evaluated by R, are still in their types (or, for closures,
   which R only knows as functions, not out of them). *)

open Mlsem.Types

let types = [
  "int1"; "^int1"; "INT1 \\ ^INT1"; "int"; "INT & ~vec1"; "int1 & ~0L";
  "42."; "^dbl1"; "dbl1"; "dbl & ~dbl1";
  "\"a\" | \"b\""; "chr1 \\ \"a\""; "^chr1"; "CHR & ~vec1";
  "lgl1"; "tt"; "^lgl1 | null"; "clx1"; "raw";
  "null"; "int | null"; "~null"; "any"; "vec"; "vec1";
  "{ }"; "{ a: int1, b: chr1? }"; "{ int1 }"; "list"; "{ a: { b: tt } }";
  "{ a: dbl1 }<foo>"; "INT<factor> with { levels: CHR }"; "<foo, bar>";
  "dbl with { names: chr }"; "int with { dim: INT }";
  "env"; "sym"; "lang"; "(x: dbl1) -> chr1";
  "v('a)"; "'a"; "{ a: 'a, b: 'a }"; "v1('a) | null";
  "empty"; "dbl1 & chr1"; "int1 & chr1 & ~^INT1";
]

let n = 4

(* For each type, its [n] draws: the instantiated type and the value. *)
let draws () =
  types |> List.mapi (fun i s ->
    let st = Random.State.make [| i |] in
    let ty = Lang.Defs.ty s in
    s, List.init n (fun _ ->
      Typr.Gen.sample_poly st ty
      |> Option.map (fun (subst, v) -> Rstt.Subst.apply subst ty, v)))

let print_draws () =
  draws () |> List.iter (fun (s, vs) ->
    print_endline s ;
    vs |> List.iter (function
      | None -> print_endline "  (none)"
      | Some (ty, v) ->
        let ok = Ty.leq (Typr.Rval.to_valty v).ty ty in
        Printf.printf "  %s%s\n" (Typr.Rval.to_r v) (if ok then "" else "  NOT IN TYPE")))

let values () =
  draws () |> List.concat_map (fun (s, vs) -> List.filter_map (Option.map (fun d -> s, d)) vs)

let print_cases () =
  values () |> List.iter (fun (_, (_, v)) ->
    print_endline (Yojson.Safe.to_string (`Assoc ["src", `String (Typr.Rval.to_r v)])))

let compare_descs descs =
  let descs = In_channel.with_open_text descs In_channel.input_lines in
  let values = values () in
  if List.length descs <> List.length values then
    Printf.printf "%d values, %d descriptions\n" (List.length values) (List.length descs)
  else begin
    let bad = List.combine values descs |> List.filter (fun ((s, (ty, v)), desc) ->
      match Typr.Valty.(mem (of_json (Yojson.Safe.from_string desc)) ty) with
      | In | Unknown -> false
      | Out -> Printf.printf "%s: %s\n  R: %s\n" s (Typr.Rval.to_r v) desc ; true) in
    Printf.printf "%d values, %d out of their type in R\n" (List.length values) (List.length bad)
  end

let () =
  Lang.Driver.setup () ;
  let run () =
    match Sys.argv with
    | [| _ ; "--cases" |] -> print_cases ()
    | [| _ ; "--compare" ; _ ; descs |] -> compare_descs descs
    | _ -> print_draws ()
  in
  PEnv.sequential_handler PEnv.empty run () |> ignore
