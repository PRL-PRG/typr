(** Type-directed fuzzing of the inferred types ([--fuzz-runner]).

    For each function of the package that got a type, R definition or
    linked [.Call] entry point, draw calls inside its domain ({!Gen}) and
    just outside ({!Mutate}), run them in R ([fuzz/harness.R], through the
    runner command, which is where the sandbox goes), and check what comes
    back:
    - inside the domain, the result must be in the result type predicted for
      these arguments ([ok]; when that type is empty, the call must fail);
      otherwise the type is [unsound], or [unknown]
      when the type of the result is not exact enough to tell ({!Valty}).
      Errors are possible anyway: [error_pkg] when the package raises it
      ([stop] in its own code, [match.arg], [stopifnot], or the C code of an
      entry point), [error_type] when a function of another namespace or a
      primitive does, which hints at a type that lets through a value that
      base R refuses;
    - outside, the function should fail ([rejected]). When it returns
      ([accepted]), the domain is smaller than it could be, and the change
      to the call tells where; [not_forced] when the changed argument was
      never evaluated, which proves nothing.
    Whatever the side: [timeout], [crash] (R died: the harness resumes
    after it), [invalid] (no R value could be built for the arguments, or
    the harness failed on the sample) and [not_run] (the runner stopped
    before reaching it).

    Calls are drawn from a random state seeded with the seed, the package
    and the name of the function: the samples of a function stay the same
    from a run to the next, whatever else changed in the package. *)

open Mlsem.Types

type options = {
  runner : string ;      (* command prefix the harness is run with, e.g. [Rscript] *)
  harness : string ;     (* directory of [harness.R] and [describe.R] *)
  samples : int ;        (* calls per function, inside and outside each *)
  seed : int ;
  dir : string ;         (* where the samples and results files go *)
  call_timeout : float ; (* seconds, per call *)
}

(* A function to fuzz: an R definition, or an entry point with its C
   symbol, and its type, of which the upper bound is used. *)
type target = { name : string ; symbol : string option ; gty : GTy.t ; ty : Ty.t }

let target_of_gty name symbol gty = { name ; symbol ; gty ; ty = GTy.ub gty }

(* Whether the type has distinct bounds. Only computed for the functions
   fuzzed: comparing types can be costly. *)
let lb_ne_ub t = not (Ty.equiv (GTy.lb t.gty) t.ty)

(* Function types that can be called: some arrow, and some result. *)
let fuzzable t =
  let c = if Ty.leq t.ty Rstt.Attr.any then Rstt.Attr.proj_content t.ty else t.ty in
  not (Ty.is_empty c) && Ty.leq c Arrow.any && not (Report.degenerate t.ty)

(* ===== Samples ===== *)

type side = In of Ty.t (* the expected result type *) | Out of Mutate.mutant

type sample = { id : int ; target : target ; call : Gen.call ; side : side }

let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: l -> x :: take (n - 1) l

let dedup key l =
  let seen = Hashtbl.create 16 in
  List.filter (fun x -> let k = key x in
    if Hashtbl.mem seen k then false else (Hashtbl.add seen k () ; true)) l

(* The calls for [t], inside then outside its domain. *)
let draw opts pkg t =
  let st = Random.State.make [| opts.seed ; Hashtbl.hash pkg ; Hashtbl.hash t.name |] in
  let positional = t.symbol <> None in
  let ins =
    List.init opts.samples (fun _ -> Gen.gen_call ~positional st t.ty)
    |> List.filter_map Fun.id
    |> dedup (fun (s : Gen.sample) -> Gen.call_to_r s.call) in
  let outs = match ins with
    | [] -> []
    | _ ->
      let per = (opts.samples + List.length ins - 1) / List.length ins in
      ins |> List.concat_map (fun (s : Gen.sample) -> Mutate.mutants ~n:per st t.ty s.call)
      |> dedup (fun (m : Mutate.mutant) -> Gen.call_to_r m.call)
      |> take opts.samples in
  List.map (fun (s : Gen.sample) -> s.call, In s.expected) ins
  @ List.map (fun (m : Mutate.mutant) -> m.call, Out m) outs

let sample_json s : Yojson.Safe.t =
  let arg name v = `Assoc [ "name", name ; "src", `String (Rval.to_r v) ] in
  let args = List.map (arg `Null) s.call.pos
             @ List.map (fun (n, v) -> arg (`String n) v) s.call.named in
  `Assoc [ "id", `Int s.id ;
           "kind", `String (if s.target.symbol = None then "r" else "native") ;
           "fun", `String s.target.name ;
           "symbol", (match s.target.symbol with Some c -> `String c | None -> `Null) ;
           "args", `List args ]

(* ===== Running the harness ===== *)

let read_lines path =
  if Sys.file_exists path then In_channel.with_open_text path In_channel.input_lines else []

let field k = function `Assoc l -> (try List.assoc k l with Not_found -> `Null) | _ -> `Null

(* Runs the harness on the samples file until it has gone through every
   sample: when R dies, the results end with a started sample, and the
   harness resumes after it. What R prints, crash reports included, goes
   to [log]. Returns the result of each sample that has one, and the
   samples started without one (the crashes). *)
let run_harness opts pkg ~samples ~results ~log n =
  List.iter (fun f -> if Sys.file_exists f then Sys.remove f) [ results ; log ] ;
  let cmd =
    String.concat " "
      (opts.runner
       :: List.map Filename.quote
            [ Filename.concat opts.harness "harness.R" ; Filename.concat opts.harness "describe.R" ;
              pkg ; samples ; results ; Printf.sprintf "%g" opts.call_timeout ])
    ^ " >> " ^ Filename.quote log ^ " 2>&1" in
  let parse () =
    read_lines results |> List.filter_map (fun l ->
      try Some (Yojson.Safe.from_string l) with Yojson.Json_error _ -> None) in
  let rec loop runs =
    let code = Sys.command cmd in
    let lines = parse () in
    let last_started = match List.rev lines with
      | l :: _ when field "start" l = `Bool true -> true | _ -> false in
    if code <> 0 && not last_started then
      Format.printf "fuzz: the harness exited with code %d (see %s)@." code log ;
    if last_started && runs < n then loop (runs + 1) else lines
  in
  let lines = loop 1 in
  let outcomes = Hashtbl.create n and started = Hashtbl.create n in
  lines |> List.iter (fun l ->
    match field "id" l with
    | `Int id ->
      if field "start" l = `Bool true then Hashtbl.replace started id ()
      else Hashtbl.replace outcomes id l
    | _ -> ()) ;
  outcomes, started

(* ===== Verdicts ===== *)

(* Errors the package raises on purpose, rather than ones from the code of
   another namespace it calls. [stop] reports the call of its caller. *)
let pkg_error pkg s r =
  let str k = match field k r with `String v -> v | _ -> "" in
  s.target.symbol <> None || str "ns" = pkg
  || List.mem (str "fun") [ "match.arg" ; "stopifnot" ]

type verdict = {
  verdict : string ;
  result : Ty.t option ;          (* the type of the value returned *)
  r : Yojson.Safe.t ;             (* the harness's record *)
}

let judge pkg s outcomes started =
  let v ?result verdict r = { verdict ; result ; r } in
  match Hashtbl.find_opt outcomes s.id with
  | None -> v (if Hashtbl.mem started s.id then "crash" else "not_run") `Null
  | Some r ->
    let forced () =
      match s.side, field "forced" r with
      | Out { arg = Some i ; _ }, `List fs -> List.nth_opt fs i <> Some (`Bool false)
      | _ -> true in
    match field "outcome" r, s.side with
    | `String "value", side ->
      (match Valty.of_json (field "value" r) with
       | exception Valty.Bad_description _ -> v "invalid" r
       | value ->
         let result = value.ty in
         match side with
         | In expected ->
           (match Valty.mem value expected with
            | In -> v ~result "ok" r | Out -> v ~result "unsound" r | Unknown -> v ~result "unknown" r)
         | Out _ -> v ~result (if forced () then "accepted" else "not_forced") r)
    (* No result predicted: the function must fail. *)
    | `String "error", In expected when Ty.is_empty expected -> v "ok" r
    | `String "error", In _ -> v (if pkg_error pkg s r then "error_pkg" else "error_type") r
    | `String "error", Out _ -> v "rejected" r
    | `String "timeout", _ -> v "timeout" r
    | _ -> v "invalid" r

(* ===== Reporting ===== *)

let verdicts = [ "ok" ; "unsound" ; "unknown" ; "error_pkg" ; "error_type" ;
                 "rejected" ; "accepted" ; "not_forced" ;
                 "timeout" ; "crash" ; "invalid" ; "not_run" ]

let strings k r = match field k r with
  | `List l -> List.filter_map (function `String s -> Some s | `Bool b -> Some (string_of_bool b) | _ -> None) l
  | _ -> []

let str k r = match field k r with `String s -> Report.S s | _ -> Report.N

let pp_ty ty = Report.one_line Ty.pp ty

let report_sample rep s j =
  let module R = Report in
  let param, op = match s.side with
    | In _ -> R.N, R.N
    | Out m -> R.S m.param, R.S m.op in
  R.emit rep "fuzz_sample"
    [ ("def", R.S s.target.name) ;
      ("kind", R.S (if s.target.symbol = None then "r" else "native")) ;
      ("id", R.I s.id) ;
      ("dir", R.S (match s.side with In _ -> "in" | Out _ -> "out")) ;
      ("verdict", R.S j.verdict) ;
      ("args", R.S (Gen.call_to_r s.call)) ;
      ("param", param) ; ("op", op) ;
      ("expected", (match s.side with In e -> R.S (pp_ty e) | Out _ -> R.N)) ;
      ("result", (match j.result with Some t -> R.S (pp_ty t) | None -> R.N)) ;
      ("error_class", (match strings "class" j.r with c :: _ -> R.S c | [] -> R.N)) ;
      ("error_message", str "message" j.r) ;
      ("error_fun", str "fun" j.r) ; ("error_ns", str "ns" j.r) ;
      ("forced", R.L (strings "forced" j.r)) ;
      ("effects", R.L (strings "effects" j.r)) ;
      ("warnings", R.L (strings "warnings" j.r)) ;
      ("elapsed_sec", (match field "elapsed" j.r with
         | `Float f -> R.F f | `Int i -> R.F (float_of_int i) | _ -> R.N)) ]

(* The first sample with this verdict, as a counterexample. *)
let example judged verdict =
  List.find_opt (fun (_, j) -> j.verdict = verdict) judged

let print_example (s, j) =
  let what = match s.side, j.result with
    | In e, Some r -> Printf.sprintf "returned %s, expected %s" (pp_ty r) (pp_ty e)
    | Out m, Some r -> Printf.sprintf "%s %s, returned %s" m.param m.op (pp_ty r)
    | _ -> "" in
  Format.printf "  %s: %s %s@." j.verdict (Gen.call_to_r s.call) what

let report_def rep t judged =
  let module R = Report in
  let count v = List.length (List.filter (fun (_, j) -> j.verdict = v) judged) in
  let n_in = List.length (List.filter (fun (s, _) -> match s.side with In _ -> true | _ -> false) judged) in
  let effects = judged |> List.concat_map (fun (_, j) -> strings "effects" j.r)
                |> List.sort_uniq String.compare in
  let exported = judged |> List.find_map (fun (_, j) ->
    match field "exported" j.r with `Bool b -> Some b | _ -> None) in
  R.emit rep "fuzz_def"
    ([ ("def", R.S t.name) ;
       ("kind", R.S (if t.symbol = None then "r" else "native")) ;
       ("type", R.S (pp_ty t.ty)) ;
       ("n_in", R.I n_in) ; ("n_out", R.I (List.length judged - n_in)) ;
       ("ungeneratable", R.B (n_in = 0)) ; ("lb_ne_ub", R.B (lb_ne_ub t)) ;
       ("exported", (match exported with Some b -> R.B b | None -> R.N)) ;
       ("effects", R.L effects) ]
     @ List.map (fun v -> ("n_" ^ v, R.I (count v))) verdicts) ;
  let counts = verdicts |> List.filter_map (fun v ->
    match count v with 0 -> None | n -> Some (Printf.sprintf "%s %d" v n)) in
  Format.printf "%s: %s%s@." t.name
    (if n_in = 0 then "no call inside the domain; " else "")
    (String.concat ", " counts) ;
  List.iter (fun v -> Option.iter print_example (example judged v)) [ "unsound" ; "accepted" ] ;
  if effects <> [] then Format.printf "  effects: %s@." (String.concat ", " effects)

(* ===== Driver ===== *)

(* The package name, from its DESCRIPTION, or else its directory. *)
let package_name root =
  let desc = Filename.concat root "DESCRIPTION" in
  read_lines desc |> List.find_map (fun l ->
    match String.index_opt l ':' with
    | Some i when String.trim (String.sub l 0 i) = "Package" ->
      Some (String.trim (String.sub l (i + 1) (String.length l - i - 1)))
    | _ -> None)
  |> Option.value ~default:(Filename.basename root)

(* [targets] in the order of their definitions: a name defined twice is
   the later definition. *)
let run opts rep root targets =
  let pkg = package_name root in
  let targets =
    List.rev targets |> dedup (fun t -> t.name) |> List.rev |> List.filter fuzzable in
  let next = ref 0 in
  let samples = targets |> List.concat_map (fun t ->
    let calls =
      try draw opts pkg t
      with e ->
        Format.printf "fuzz: no calls for %s (%s)@." t.name (Printexc.to_string e) ; [] in
    calls |> List.map (fun (call, side) -> incr next ; { id = !next ; target = t ; call ; side })) in
  if not (Sys.file_exists opts.dir) then Sys.mkdir opts.dir 0o755 ;
  let path ext = Filename.concat opts.dir (pkg ^ ext) in
  let samples_file = path ".samples.jsonl" and results_file = path ".results.jsonl"
  and log = path ".harness.log" in
  Out_channel.with_open_text samples_file (fun oc ->
    samples |> List.iter (fun s ->
      output_string oc (Yojson.Safe.to_string (sample_json s)) ; output_char oc '\n')) ;
  let outcomes, started =
    if samples = [] then Hashtbl.create 0, Hashtbl.create 0
    else run_harness opts pkg ~samples:samples_file ~results:results_file ~log (List.length samples) in
  let judged = samples |> List.map (fun s ->
    let j = judge pkg s outcomes started in
    report_sample rep s j ; s, j) in
  targets |> List.iter (fun t ->
    report_def rep t (List.filter (fun (s, _) -> s.target == t) judged))
