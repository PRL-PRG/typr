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
  | PAst.Fundef (_, name, _, _) | PAst.MacroDef (name, _, _) -> StrSet.add name acc
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

(* A function definition or a function-like macro: both are typed. *)
let is_fundef = function _, (PAst.Fundef _ | PAst.MacroDef _) -> true | _ -> false

type fundef = {
  file : string ;
  past : PAst.top_level_unit ;
  name : string ;
  (* A function-like macro: it may share [name] with a function, and both are
     scheduled. *)
  macro : bool ;
  (* The package's own functions this one calls or refers to. *)
  calls : string list ;
}

(* [PAst.extract_calls_from_expr] knows the C-specific ways a function name can
   appear -- a call target, a bare identifier stored in a struct field, [&fn]
   handed to a callback registration, either of those behind a cast -- so a
   function reached only as a value is not mistaken for dead code. *)
let calls_of ~fun_names = function
  | _, (PAst.Fundef (_, _, _, body) | PAst.MacroDef (_, _, body)) ->
    PAst.extract_calls_from_expr ~fun_names body
    |> List.filter (fun n -> StrSet.mem n fun_names)
    |> List.sort_uniq String.compare
  | _ -> []

(* The top-level function definitions and function-like macros, in source
   order. A function defined twice (a prototype and its definition, or a
   definition per translation unit) yields a single entry, the one carrying a
   body; likewise a macro. A macro and a function of the same name are two
   entries. *)
let fundefs ~fun_names pasts =
  let is_macro = function _, PAst.MacroDef _ -> true | _ -> false in
  let seen = Hashtbl.create 64 in
  pasts
  |> List.concat_map (fun (file, past) ->
      past |> List.filter (fun item -> is_fundef item)
           |> List.map (fun past ->
               { file ; past ; name = PAst.top_level_unit_name past ;
                 macro = is_macro past ; calls = calls_of ~fun_names past }))
  |> List.filter (fun d ->
      (* Keep the first entry for a name, but let a definition replace a
         prototype seen earlier. *)
      let key = (d.macro, d.name) in
      match Hashtbl.find_opt seen key with
      | Some `Def -> false
      | Some `Decl when is_declaration d.past -> false
      | Some `Decl -> Hashtbl.replace seen key `Def ; true
      | None ->
        Hashtbl.add seen key (if is_declaration d.past then `Decl else `Def) ;
        true)
  |> (fun defs ->
      (* A definition replacing a prototype leaves the prototype in the list;
         drop it now that we know a body exists. *)
      let kept = Hashtbl.create 64 in
      defs |> List.iter (fun d -> Hashtbl.replace kept (d.macro, d.name) d) ;
      defs |> List.filter (fun d -> Hashtbl.find kept (d.macro, d.name) == d))

(* The functions reachable from [roots], or all of them when there is no root
   to start from (a package whose R side we could not read). *)
let reachable ~roots defs =
  match roots with
  | [] -> defs
  | roots ->
    (* Calls name definitions, not namespaces: a name reaches both the
       function and the macro it may denote. *)
    let by_name =
      List.fold_left (fun m d ->
          StrMap.update d.name (fun l -> Some (d :: Option.value ~default:[] l)) m)
        StrMap.empty defs in
    let seen = Hashtbl.create 64 in
    let rec visit n =
      if not (Hashtbl.mem seen n) then begin
        Hashtbl.add seen n () ;
        StrMap.find_opt n by_name |> Option.value ~default:[]
        |> List.iter (fun d -> List.iter visit d.calls)
      end
    in
    List.iter visit roots ;
    List.filter (fun d -> Hashtbl.mem seen d.name) defs
