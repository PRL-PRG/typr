(** Dependency analysis of the native side of a package.

    The mirror image of {!R_deps}: TypR resolves the dependencies of both
    languages itself, so that functions of either are ordered -- and bounded by
    a timeout -- the same way, by the same code. *)

open R_c_typing

module StrSet = PAst.StrSet
module StrMap = Map.Make (String)

(* Every function the translation units define, headers pulled in by an
   [Include] item too: a name defined there is still a name a body can refer
   to, even though only the top-level ones are scheduled below. *)
let rec add_fun_name acc (_, u) =
  match u with
  | PAst.Fundef (_, name, _, _) -> StrSet.add name acc
  | PAst.Include items -> List.fold_left add_fun_name acc items
  | _ -> acc

let fun_names pasts =
  List.fold_left
    (fun acc (_, past) -> List.fold_left add_fun_name acc past)
    StrSet.empty pasts

(* A function definition with no body, i.e. a prototype. A real definition of
   the same name takes precedence over it. *)
let is_declaration = function
  | _, PAst.Fundef (_, _, _, (_, PAst.Seq [])) -> true
  | _ -> false

let is_fundef = function _, PAst.Fundef _ -> true | _ -> false

type fundef = {
  file : string ;
  past : PAst.top_level_unit ;
  name : string ;
  (* The package's own functions this one calls or refers to. *)
  calls : string list ;
}

(* [PAst.extract_calls_from_expr] knows the C-specific ways a function name can
   appear -- a call target, a bare identifier stored in a struct field, [&fn]
   handed to a callback registration, either of those behind a cast -- so a
   function reached only as a value is not mistaken for dead code. *)
let calls_of ~fun_names = function
  | _, PAst.Fundef (_, _, _, body) ->
    PAst.extract_calls_from_expr ~fun_names body
    |> List.filter (fun n -> StrSet.mem n fun_names)
    |> List.sort_uniq String.compare
  | _ -> []

(* The top-level function definitions, in source order. A name defined twice
   (a prototype and its definition, or a definition per translation unit)
   yields a single entry, the one carrying a body. *)
let fundefs ~fun_names pasts =
  let seen = Hashtbl.create 64 in
  pasts
  |> List.concat_map (fun (file, past) ->
      past |> List.filter (fun item -> is_fundef item)
           |> List.map (fun past ->
               { file ; past ; name = PAst.top_level_unit_name past ;
                 calls = calls_of ~fun_names past }))
  |> List.filter (fun d ->
      (* Keep the first entry for a name, but let a definition replace a
         prototype seen earlier. *)
      match Hashtbl.find_opt seen d.name with
      | Some `Def -> false
      | Some `Decl when is_declaration d.past -> false
      | Some `Decl -> Hashtbl.replace seen d.name `Def ; true
      | None ->
        Hashtbl.add seen d.name (if is_declaration d.past then `Decl else `Def) ;
        true)
  |> (fun defs ->
      (* A definition replacing a prototype leaves the prototype in the list;
         drop it now that we know a body exists. *)
      let kept = defs |> List.fold_left (fun acc d -> StrMap.add d.name d acc) StrMap.empty in
      defs |> List.filter (fun d -> (StrMap.find d.name kept) == d))

(* The functions reachable from [roots], or all of them when there is no root
   to start from (a package whose R side we could not read). *)
let reachable ~roots defs =
  match roots with
  | [] -> defs
  | roots ->
    let by_name = defs |> List.map (fun d -> (d.name, d)) |> StrMap.of_list in
    let seen = Hashtbl.create 64 in
    let rec visit n =
      if not (Hashtbl.mem seen n) then begin
        Hashtbl.add seen n () ;
        match StrMap.find_opt n by_name with
        | Some d -> List.iter visit d.calls
        | None -> ()
      end
    in
    List.iter visit roots ;
    List.filter (fun d -> Hashtbl.mem seen d.name) defs
