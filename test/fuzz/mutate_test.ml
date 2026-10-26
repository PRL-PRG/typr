(* Calls just outside function domains: for a few in-domain calls per
   signature, their mutants, with the parameter and the change. Every
   mutant must lie outside the domain of every instance. *)

open Mlsem.Types

let sigs = [
  "(x: int1, y: chr1?) -> dbl1";
  "(x: dbl, ...: chr1) -> null";
  "((x: int1) -> int1) & ((x: chr1) -> chr1)";
  "(x: 'a, y: 'a?) -> 'a";
  "(x: v('p)) -> v('p)";
  "(x: lgl1, y: INT<factor> with { levels: CHR }) -> any";
  "(a: { b: tt, c: dbl? }) -> null";
  "(f: (x: dbl1) -> dbl1, n: ^int1) -> dbl";
  "(x: dbl with { names: chr }) -> null";
  "(x: any) -> null";
]

let calls = 2

(* The mutants of [calls] calls per signature. *)
let draws () =
  sigs |> List.mapi (fun i s ->
    let st = Random.State.make [| i |] in
    let fty = Lang.Defs.ty s in
    s, fty, List.init calls (fun _ ->
      Typr.Gen.gen_call st fty
      |> Option.map (fun (c : Typr.Gen.sample) -> c.call, Typr.Mutate.mutants st fty c.call)))

let print_draws () =
  draws () |> List.iter (fun (s, fty, cs) ->
    print_endline s ;
    let dom = Typr.Gen.top_instance (Rstt.Attr.proj_content fty) |> Arrow.domain in
    cs |> List.iter (function
      | None -> print_endline "  (none)"
      | Some (c, ms) ->
        Printf.printf "  %s\n" (Typr.Gen.call_to_r c) ;
        ms |> List.iter (fun (m : Typr.Mutate.mutant) ->
          let inside = Ty.leq (Typr.Gen.call_type m.call) dom in
          Printf.printf "    %s %s: %s%s\n" m.param m.op (Typr.Gen.call_to_r m.call)
            (if inside then "  IN DOMAIN" else ""))))

(* The changed arguments. *)
let values () =
  draws () |> List.concat_map (fun (_, _, cs) ->
    cs |> List.concat_map (function
      | None -> []
      | Some (_, ms) -> ms |> List.filter_map (fun (m : Typr.Mutate.mutant) ->
        let args = m.call.pos @ List.map snd m.call.named in
        Option.map (List.nth args) m.arg)))

let print_cases () =
  values () |> List.iter (fun v ->
    print_endline (Yojson.Safe.to_string (`Assoc ["src", `String (Typr.Rval.to_r v)])))

(* What R sees of a value: not the domains of the fuzzer's closures. *)
let rec seen : Typr.Rval.t -> Typr.Rval.t = function
  | Fun f -> Fun { f with doms = [] }
  | List l -> List (List.map (fun (n, v) -> n, seen v) l)
  | With w -> With { w with v = seen w.v ; attrs = List.map (fun (n, a) -> n, seen a) w.attrs }
  | v -> v

(* R must see the changed arguments as the mutator does: their types are
   what places the mutants outside the domain. *)
let compare_descs descs =
  let descs = In_channel.with_open_text descs In_channel.input_lines in
  let values = values () in
  if List.length descs <> List.length values then
    Printf.printf "%d values, %d descriptions\n" (List.length values) (List.length descs)
  else begin
    let bad = List.combine values descs |> List.filter (fun (v, desc) ->
      let r = Typr.Valty.of_json (Yojson.Safe.from_string desc) in
      let ocaml = Typr.Rval.to_valty (seen v) in
      if Ty.equiv r.ty ocaml.ty && r.exact = ocaml.exact then false
      else (Printf.printf "%s\n  R: %s\n" (Typr.Rval.to_r v) desc ; true)) in
    Printf.printf "%d values, %d disagreements\n" (List.length values) (List.length bad)
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
