(** Dependency analysis of the R side of a package.

    Walks the surface AST Rsem produces, so that we see exactly the program the
    type-checker will see -- no second parse, and no regex over the source. *)

open Lang

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

(* The four ways R reaches into a native library. The symbol is the *R-visible*
   name (before [Pkg.native_symbol] strips the [.fixes] prefix). *)
type native_call = { symbol : string ; convention : R_c_typing.Package.calling_convention }

let convention_of_name = function
  | ".C" -> Some R_c_typing.Package.C
  | ".Call" -> Some R_c_typing.Package.Call
  | ".Fortran" -> Some R_c_typing.Package.Fortran
  | ".External" -> Some R_c_typing.Package.External
  | _ -> None

(* [.Call(foo, ...)] names the symbol with a variable, [.Call("foo", ...)] with
   a string; both are used in the wild. *)
let symbol_of_arg = function
  | Some (PAst.Unnamed (_, PAst.Id str)) -> Some str
  | Some (PAst.Unnamed (_, PAst.Const (PAst.CStr str))) -> Some str
  | _ -> None

(* Every identifier and native call occurring in [e], nested functions
   included. Over-approximating (locals are included too) is harmless here: the
   result is only used to order definitions, and a local shadowing a top-level
   name just adds an edge that was already implied. *)
let scan_e e =
  let ids = ref StrSet.empty and natives = ref [] in
  let add_id str = ids := StrSet.add str !ids in
  let rec aux (_, e) =
    match e with
    | PAst.Id str -> add_id str
    | PAst.Const _ | PAst.Dots | PAst.DotsN _
    | PAst.Return | PAst.Break | PAst.Next -> ()
    | PAst.Unop (str, e) -> add_id str ; aux e
    | PAst.Binop (str, (e1, e2)) -> add_id str ; aux e1 ; aux e2
    | PAst.Dollar (e, _) | PAst.At (e, _) -> aux e
    | PAst.Call (f, args) ->
      (match f, args with
       | (_, PAst.Id name), first :: _ ->
         (match convention_of_name name, symbol_of_arg first with
          | Some convention, Some symbol ->
            natives := { symbol ; convention } :: !natives
          | _ -> ())
       | _ -> ()) ;
      aux f ; aux_args args
    | PAst.Subset (e, args) | PAst.Subset2 (e, args) -> aux e ; aux_args args
    | PAst.Function (_, params, e) ->
      Option.iter (List.iter (function
        | PAst.Default (_, e) -> aux e
        | PAst.NoDefault _ -> ())) params ;
      aux e
    | PAst.Ite (e, e1, e2) -> aux e ; aux e1 ; Option.iter aux e2
    | PAst.While (e1, e2) | PAst.For (_, e1, e2) -> aux e1 ; aux e2
    | PAst.Braced es -> List.iter aux es
  and aux_args args =
    args |> List.iter (function
      | None | Some (PAst.Named (_, None)) -> ()
      | Some (PAst.Unnamed e) | Some (PAst.Named (_, Some e)) -> aux e)
  in
  aux e ; !ids, List.rev !natives

(* One top-level definition, with what it needs. [name] is [None] for a
   top-level statement that binds nothing. *)
type def = {
  name : string option ;
  uses : StrSet.t ;
  natives : native_call list ;
}

let def_of_past past =
  let uses, natives = scan_e past in
  { name = Driver.toplevel_name past ; uses ; natives }

let defs_of_program (prog : PAst.t) = List.map def_of_past prog

(* All the native symbols an R program reaches, R-visible name first. *)
let natives_of_defs defs =
  defs |> List.concat_map (fun d -> d.natives)
  |> List.fold_left (fun acc n -> StrMap.add n.symbol n.convention acc) StrMap.empty
