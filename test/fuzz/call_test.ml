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

(* Calls drawn from each arrow of overloaded signatures: each must lie in
   the domain of its arrow. *)
let per_arrow = [
  "((x: int1) -> int1) & ((x: chr1) -> chr1)";
  "((x: 'a & null) -> null) & ((x: v('p)) -> v('p)) & ((x: { a: dbl }) -> dbl)";
]

let run_per_arrow () =
  per_arrow |> List.iteri (fun i s ->
    let st = Random.State.make [| 100 + i |] in
    let fty = Lang.Defs.ty s in
    Printf.printf "%s  (per arrow)\n" s ;
    Typr.Gen.arrows fty |> List.iteri (fun k (dom, _) ->
      for _ = 1 to 2 do
        match Typr.Gen.gen_call ~arrow:k st fty with
        | None -> Printf.printf "  arrow %d: (none)\n" k
        | Some { subst ; call ; expected } ->
          let ok = Ty.leq (Typr.Gen.call_type call) (Rstt.Subst.apply subst dom) in
          Printf.printf "  arrow %d: %s%s\n    -> %s\n" k (Typr.Gen.call_to_r call)
            (if ok then "" else "  NOT IN ITS ARROW") (Typr.Report.one_line Ty.pp expected)
      done))

let () =
  Lang.Driver.setup () ;
  PEnv.sequential_handler PEnv.empty (fun () -> run () ; run_per_arrow ()) () |> ignore
