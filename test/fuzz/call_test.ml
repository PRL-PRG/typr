(* Argument lists drawn from function domains, with the result type each
   call must have: a few calls per signature, with a fixed seed. *)

open Mlsem.Types

let sigs = [
  false, "(x: int1, y: chr1?) -> dbl1";
  false, "(x: dbl, ...: chr1) -> null";
  false, "(...: any) -> null";
  false, "((x: int1) -> int1) & ((x: chr1) -> chr1)";
  false, "(x: 'a, y: 'a?) -> 'a";
  false, "(x: v('p)) -> v('p)";
  false, "(x: lgl1, y: INT<factor> with { levels: CHR }) -> any";
  false, "(a: { b: tt, c: dbl? }) -> null";
  false, "(f: (x: dbl1) -> dbl1, n: ^int1) -> dbl";
  true, "(x1: INT, x2: CHR1) -> null";
  false, "(x: empty) -> null";
  false, "(x: 'a & null) -> null & 'a";
  false, "(p: externalptr, f: (x: dbl) -> empty) -> null";
]

let n = 3

let run () =
  sigs |> List.iteri (fun i (positional, s) ->
    let st = Random.State.make [| i |] in
    let fty = Lang.Defs.ty s in
    Printf.printf "%s%s\n" s (if positional then "  (positional)" else "") ;
    for _ = 1 to n do
      match Typr.Gen.gen_call ~positional st fty with
      | None -> print_endline "  (none)"
      | Some { subst ; call ; expected } ->
        let dom = Rstt.Subst.apply subst fty |> Rstt.Attr.proj_content |> Arrow.domain in
        let ok = Ty.leq (Typr.Gen.call_type call) dom in
        Printf.printf "  %s%s\n    -> %s\n" (Typr.Gen.call_to_r call)
          (if ok then "" else "  NOT IN DOMAIN") (Typr.Report.one_line Ty.pp expected)
    done)

let () =
  Lang.Driver.setup () ;
  PEnv.sequential_handler PEnv.empty run () |> ignore
