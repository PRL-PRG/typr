(** Linking the two sides.

    NativeSem types a routine reached through [.Call] as a function over a C
    argument *tuple* -- [[T1, T2] -> R]. On the R side the same routine is
    called as an ordinary R closure, whose domain is an R argument record. This
    module bridges the two: it rewrites the calls, and adapts the types. *)

open Lang
open Mlsem.Types

module StrMap = Map.Make(String)

(* ===== Types ===== *)

(* [Mlsem.Types.Ty] and [Rstt.Ty] denote the same [Sstt.Ty.t], but only the
   latter exposes the optional/field constructors. *)
let field ty = Rstt.(Ty.O.required ty |> Ty.F.mk_descr)
let absent = Rstt.(Ty.F.mk_descr Ty.O.absent)

(* [x1: T1, ..., xn: Tn] as a definition-site R argument: exactly [n]
   positional parameters, no [...]. *)
let arg_of_components tys =
  Rstt.Arg.mk
    { pos_named = List.mapi (fun i ty -> (Printf.sprintf "x%d" (i + 1), field ty)) tys ;
      pos_tl = absent ; named = [] ; named_tl = absent }

(* The R argument type matching a C argument tuple. A domain that is a union of
   tuples (of possibly different arities) yields the union of the corresponding
   argument types. *)
let arg_of_tuple dom =
  let comps, _ = Tuple.decompose dom in
  comps
  |> List.concat_map (fun (_arity, lines) -> List.map arg_of_components lines)
  |> Ty.disj

let fun_ty = Arrow.any |> Rstt.Attr.mk_content

(* [r_type_of_native ty] is the type of the R binding that stands for a native
   routine whose inferred type is [ty]. [None] when [ty] is not a function type
   (e.g. inference failed and left a global). *)
let r_type_of_native ty =
  (* A routine inferred from its body carries R attributes ([... -> ...]);
     one typed from its C signature does not ([... --> ...]). *)
  let content =
    if Ty.leq ty (Rstt.Attr.any) then Rstt.Attr.proj_content ty else ty in
  if Ty.is_empty content || not (Ty.leq content Arrow.any) then None
  else
    let arrows =
      Arrow.dnf content
      |> List.map (List.map (fun (dom, codom) -> (arg_of_tuple dom, codom)))
      |> Arrow.of_dnf
    in
    if Ty.is_empty arrows then None else Some (Rstt.Attr.mk_content arrows)

let is_fun_ty ty = Ty.leq ty fun_ty && not (Ty.is_empty ty)

(* ===== Calls ===== *)

(* [.Call(foo, a, b)] is an application of the routine [foo] to [a] and [b].
   Rewriting it to [foo(a, b)] is what lets the R type-checker use the type
   NativeSem inferred for [foo] -- it has no built-in knowledge of [.Call].
   Only symbols [known] to the native side are rewritten; the others are left
   alone, so they keep failing visibly rather than silently changing meaning.

   Only [.Call] is rewritten. The other conventions do not pass their arguments
   through: [.C] copies them and returns the (possibly modified) argument list,
   and [.External] packs everything into a single LANGSXP, so the R-level type
   of such a call is not the type of the C routine. *)
let rewrite_native_calls known (prog : PAst.t) : PAst.t =
  let rec aux (pos, e) =
    let e =
      match e with
      | PAst.Call (((_, PAst.Id name) as f), (first :: rest as args)) ->
        (match R_deps.convention_of_name name, R_deps.symbol_of_arg first with
         | Some R_c_typing.Package.Call, Some symbol when known symbol ->
           PAst.Call ((pos, PAst.Id symbol), List.map aux_arg rest)
         | _ -> PAst.Call (aux f, List.map aux_arg args))
      | PAst.Call (f, args) -> PAst.Call (aux f, List.map aux_arg args)
      | PAst.Unop (str, e) -> PAst.Unop (str, aux e)
      | PAst.Binop (str, (e1, e2)) -> PAst.Binop (str, (aux e1, aux e2))
      | PAst.Dollar (e, a) -> PAst.Dollar (aux e, a)
      | PAst.At (e, a) -> PAst.At (aux e, a)
      | PAst.Subset (e, args) -> PAst.Subset (aux e, List.map aux_arg args)
      | PAst.Subset2 (e, args) -> PAst.Subset2 (aux e, List.map aux_arg args)
      | PAst.Function (l, params, e) ->
        PAst.Function (l, Option.map (List.map aux_param) params, aux e)
      | PAst.Ite (e, e1, e2) -> PAst.Ite (aux e, aux e1, Option.map aux e2)
      | PAst.While (e1, e2) -> PAst.While (aux e1, aux e2)
      | PAst.For (i, e1, e2) -> PAst.For (i, aux e1, aux e2)
      | PAst.Braced es -> PAst.Braced (List.map aux es)
      | (PAst.Const _ | PAst.Id _ | PAst.Dots | PAst.DotsN _
        | PAst.Return | PAst.Break | PAst.Next) as e -> e
    in
    (pos, e)
  and aux_arg = function
    | None -> None
    | Some (PAst.Unnamed e) -> Some (PAst.Unnamed (aux e))
    | Some (PAst.Named (i, e)) -> Some (PAst.Named (i, Option.map aux e))
  and aux_param = function
    | PAst.NoDefault i -> PAst.NoDefault i
    | PAst.Default (i, e) -> PAst.Default (i, aux e)
  in
  List.map aux prog
