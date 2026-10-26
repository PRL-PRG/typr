(** Type-directed generation of R values.

    [sample st ty] draws an R value of type [ty]. Generation is approximate
    and validation exact: a candidate is built by walking the structure of
    [ty] (its kinds, vector modes, list fields, classes, attributes),
    ignoring what is hard to follow (negations, intersections of atoms), and
    is kept only if its type ({!Rval.to_valty}) is a subtype of [ty]. Up to
    [tries] candidates are drawn; [None] means that none was in [ty], which
    the fuzzer reports rather than hides.

    Type variables are instantiated first ({!instantiate}), with ground types
    drawn from small pools; [sample_poly] does both. *)

open Rstt

type st = Random.State.t

let pick st = function
  | [] -> None
  | l -> Some (List.nth l (Random.State.int st (List.length l)))

let coin st = Random.State.bool st

(* ===== Type variables ===== *)

let mode_pool = [ Prim.Int.any ; Prim.Dbl.any ; Prim.Chr.any ; Prim.Lgl.any ]

(* Instances for a variable: a whole vector mode, for the variables under
   [v(..)], or the type of an R value. The prims are whole (with [NA]):
   non-scalar vectors can only be typed by a whole mode (cf. {!Valty}). *)
let var_pool =
  let vec m = Attr.mk_content (Vec.mk (Vec.Vector (Prim.mk m))) in
  let scalar m = Attr.mk_content (Vec.mk (Vec.Scalar (Prim.mk m))) in
  List.map Prim.mk mode_pool
  @ [ vec Prim.Dbl.any ; scalar Prim.Chr.any' ; Null.any ; Attr.mk_content Lst.any ]

(* Rows for row variables: no further field (a list with no other element,
   a value with no other attribute), or anything. *)
let row_pool = [ Row.all_fields (Ty.F.mk_descr Ty.O.absent) ; Row.any ]

(* The R values: [NULL] and symbols, and every value that may carry
   attributes. *)
let r_values = Ty.disj [ Null.any ; Sym.any ; Attr.any ]

(* A substitution of every variable of [ty], each drawn among the instances
   that add values of [within] to those [ty] has when the variable is empty:
   a variable under [v(..)] gets a mode, a variable standing for a value gets
   a value type. When no instance does, any instance that leaves [ty] with
   values of [within] will do. The variables are instantiated one at a time,
   so that the check sees the instances already drawn. *)
let instantiate ?(within = r_values) st ty =
  let try_pool apply empty pool v ty =
    let without = Ty.cap (apply v empty ty) within in
    let insts = pool |> List.filter_map (fun inst ->
      let ty' = apply v inst ty in
      let vals = Ty.cap ty' within in
      if Ty.is_empty vals then None else Some (inst, ty', Ty.leq vals without)) in
    let adding = List.filter (fun (_, _, same) -> not same) insts in
    pick st (if adding = [] then insts else adding)
    |> Option.map (fun (inst, ty', _) -> inst, ty')
  in
  let ty, s1 =
    Ty.vars ty |> VarSet.elements |> List.fold_left (fun (ty, s) v ->
      let apply v inst ty = Subst.apply (Subst.singleton1 v inst) ty in
      match try_pool apply Ty.empty var_pool v ty with
      | None -> ty, s
      | Some (inst, ty) -> ty, (v, inst) :: s) (ty, [])
  in
  let ty, s2 =
    Ty.row_vars ty |> RowVarSet.elements |> List.fold_left (fun (ty, s) v ->
      let apply v inst ty = Subst.apply (Subst.singleton2 v inst) ty in
      match try_pool apply Row.empty row_pool v ty with
      | None -> ty, s
      | Some (inst, ty) -> ty, (v, inst) :: s) (ty, [])
  in
  ty, Subst.of_list (List.rev s1) (List.rev s2)

(* ===== Fields ===== *)

(* A list or record field: its type, and whether it may be absent. *)
let field f =
  let a = Ty.F.get_descr f |> Ty.O.get in
  Ty.O.Atom.get a, Ty.O.Atom.is_optional a

(* The fields of the positive atoms of a list line, merged by label, and the
   merged tail. *)
let lst_fields (ps : (string, Ty.F.t) Lst.atom list) =
  let labels = ps |> List.concat_map (fun (a : _ Lst.atom) -> List.map fst a.bindings)
               |> List.sort_uniq String.compare in
  let find lbl (a : _ Lst.atom) = Option.value (List.assoc_opt lbl a.bindings) ~default:a.tl in
  let merge get = List.fold_left (fun f a -> Ty.F.cap f (get a)) Ty.F.any ps in
  List.map (fun lbl -> lbl, merge (find lbl)) labels, merge (fun a -> a.tl)

(* ===== Vector elements ===== *)

let elt_ty mode e =
  fst (Valty.scalar_prim (Rval.mode_name mode) (`Assoc (Rval.elt_json mode e)))

(* The interval endpoints and strings a prim type mentions: the boundaries
   worth trying. Rstt does not re-export the module of its destructed prims. *)
module U = Rstt__Utils

let int_bounds (lines : U.interval U.atomic_t) =
  lines |> List.concat_map (fun (l : _ U.atomic_line) ->
    l.prim |> List.concat_map (fun (lo, hi) ->
      let around = function None -> [] | Some b -> [b - 1 ; b ; b + 1] in
      around lo @ around hi))

let strings (lines : string U.atomic_t) =
  lines |> List.concat_map (fun (l : _ U.atomic_line) -> l.prim)

let safe f x = try f x with Invalid_argument _ | Not_found -> []

(* Candidate elements of [mode]: [NA], usual values, and the boundaries the
   prim type [p] mentions. *)
let elt_pool mode p =
  let open Rval in
  let ints = [0 ; 1 ; -1 ; 2 ; 42 ; 1000000 ; 2147483647 ; -2147483647] in
  NA :: match mode with
  | Lgl -> [B true ; B false]
  | Int ->
    let bounds = safe (fun p -> snd (Prim.Int.destruct (Ty.cap p Prim.Int.any)) |> int_bounds) p in
    List.map (fun i -> I i) (ints @ bounds)
  | Dbl ->
    let bounds =
      safe (fun p -> (snd (Prim.Dbl.destruct (Ty.cap p Prim.Dbl.any))).integers |> int_bounds) p in
    List.map (fun i -> D (float_of_int i)) (ints @ bounds)
    @ [D 0.5 ; D (-2.5) ; D Float.nan ; D Float.infinity ; D Float.neg_infinity]
  | Clx -> [C (0., 0.) ; C (1., -2.)]
  | Chr ->
    let strs = safe (fun p -> snd (Prim.Chr.destruct (Ty.cap p Prim.Chr.any)) |> strings) p in
    List.map (fun s -> S s) (strs @ ["a" ; "" ; "NA" ; "x y" ; "\xc3\xa9"])
  | Raw -> [I 0 ; I 255]

(* The elements of [mode] in the prim type [p]. *)
let elts_in mode p =
  let pc = Prim.destruct p in
  elt_pool mode p |> List.filter (fun e ->
    match mode, e with
    | Rval.Raw, Rval.NA -> false
    | _ -> Ty.leq (elt_ty mode e) pc)

let modes = Rval.[ Lgl, Prim.Lgl.any ; Int, Prim.Int.any ; Dbl, Prim.Dbl.any ;
                   Clx, Prim.Clx.any ; Chr, Prim.Chr.any ; Raw, Prim.Raw.any ]

let gen_vec st c =
  let lines = try Vec.destruct (Ty.cap c Vec.any) with Invalid_argument _ -> [] in
  Option.bind (pick st lines) (fun (atom, _negs) ->
    let p, len = match atom with
      | Vec.Scalar p -> p, 1
      | Vec.Vector p -> p, Option.get (pick st [0 ; 1 ; 1 ; 2 ; 5]) in
    let pc = Prim.destruct p in
    let modes = modes |> List.filter (fun (_, any) -> not (Ty.disjoint pc any)) in
    Option.bind (pick st modes) (fun (mode, _) ->
      (* Only scalars carry their element in their type: the elements of a
         longer vector are free, [NA] included. *)
      let pool = if len = 1 then elts_in mode p
        else elt_pool mode Prim.any |> List.filter (fun e -> mode <> Raw || e <> Rval.NA) in
      if pool = [] then None
      else Some (Rval.Vec (mode, List.init len (fun _ -> Option.get (pick st pool))))))

(* ===== Values ===== *)

let tries = 20

let arrows_any = Arrows.any |> Descr.mk_arrows |> Ty.mk_descr

let rec gen st depth ty : Rval.t option =
  (* Most values carry attributes: draw [NULL] and symbols less often. *)
  let kinds = [ `Null, Null.any ; `Sym, Sym.any ] @ List.init 4 (fun _ -> `Attr, Attr.any)
              |> List.filter (fun (_, any) -> not (Ty.disjoint ty any)) |> List.map fst in
  match pick st kinds with
  | None -> None
  | Some `Null -> Some Rval.Null
  | Some `Sym -> Some (Rval.Sym "x")
  | Some `Attr ->
    let lines = try Attr.destruct (Ty.cap ty Attr.any) with Invalid_argument _ -> [] in
    Option.bind (pick st lines) (fun (ps, _negs) ->
      let conj get = List.fold_left (fun t a -> Ty.cap t (get a)) Ty.any ps in
      let content = conj (fun (a : _ Attr.atom) -> a.content) in
      let classes = conj (fun (a : _ Attr.atom) -> a.classes) in
      let attrs = conj (fun (a : _ Attr.atom) -> a.attrs) in
      Option.bind (gen_content st depth content) (fun v ->
        let cls = gen_classes st classes in
        let attrs = gen_attrs st depth v attrs in
        match cls, attrs with
        | None, [] -> Some v
        | _ -> Some (Rval.With { v ; cls ; attrs })))

and gen_content st depth c =
  let kinds = [ `Vec, Vec.any ; `Lst, Lst.any ; `Env, Env.any ;
                `Lang, Lang.any ; `Fun, arrows_any ]
              |> List.filter (fun (_, any) -> not (Ty.disjoint c any)) in
  Option.bind (pick st kinds) (function
    | `Vec, _ -> gen_vec st c
    | `Lst, _ -> gen_list st depth c
    | `Env, _ -> Some Rval.Env
    | `Lang, _ -> Some (Rval.Lang "f(x)")
    | `Fun, _ ->
      (* A constant function, typed at all the domains of an intersection of
         arrows, returning a value of all their codomains. *)
      let lines = Mlsem.Types.Arrow.dnf (Ty.cap c arrows_any) in
      Option.bind (pick st lines) (fun arrows ->
        let codom = List.map snd arrows |> Ty.conj in
        sample_depth st (depth - 1) codom
        |> Option.map (fun ret -> Rval.Fun { doms = List.map fst arrows ; ret })))

and gen_list st depth c =
  let lines = try Lst.destruct (Ty.cap c Lst.any) with Invalid_argument _ -> [] in
  Option.bind (pick st lines) (fun (ps, _negs) ->
    let fields, tl = lst_fields ps in
    let exception Fail in
    try
      let named = fields |> List.filter_map (fun (lbl, f) ->
        let ty, opt = field f in
        let present = not (Ty.is_empty ty) && (not opt || (depth > 0 && coin st)) in
        if not present then (if opt then None else raise Fail)
        else match sample_depth st (depth - 1) ty with
          | Some v -> Some (Some lbl, v)
          | None -> if opt then None else raise Fail) in
      let tl_ty, _ = field tl in
      let n_tail = if depth <= 0 || Ty.is_empty tl_ty then 0 else Random.State.int st 3 in
      let tail = List.init n_tail (fun _ -> sample_depth st (depth - 1) tl_ty)
                 |> List.filter_map (Option.map (fun v -> None, v)) in
      Some (Rval.List (named @ tail))
    with Fail -> None)

and gen_classes st classes =
  match Classes.destruct classes with
  | exception Invalid_argument _ -> None
  | { pos ; unk ; _ } ->
    let names = List.map (fun (Classes.L (n, _)) -> n) in
    let maybe = names unk |> List.filter (fun _ -> coin st) in
    match names pos @ maybe with
    | [] -> None
    | cls -> Some cls

(* The attributes other than the class. [names] and [dim] must fit the
   length of the value, so they are built from it rather than from their
   type, which the validation checks anyway. *)
and gen_attrs st depth v attrs =
  let lines = try Lst.destruct (Ty.cap attrs Lst.any) with Invalid_argument _ -> [] in
  match pick st lines with
  | None -> []
  | Some (ps, _) ->
    let fields, _ = lst_fields ps in
    let len = match v with
      | Rval.Vec (_, es) -> Some (List.length es)
      | Rval.List l -> Some (List.length l)
      | _ -> None in
    fields |> List.filter_map (fun (lbl, f) ->
      let ty, opt = field f in
      if Ty.is_empty ty || (opt && coin st) then None
      else match lbl, len with
        | "names", Some n ->
          Some (lbl, Rval.Vec (Chr, List.init n (fun i -> Rval.S (Printf.sprintf "n%d" (i + 1)))))
        | "dim", Some n -> Some (lbl, Rval.Vec (Int, [I n ; I 1]))
        | "names", None | "dim", None -> None
        | _ -> sample_depth st (depth - 1) ty |> Option.map (fun a -> lbl, a))

and sample_depth st depth ty =
  let rec loop n =
    if n = 0 then None
    else match gen st depth ty with
      | Some v when Ty.leq (Rval.to_valty v).ty ty -> Some v
      | _ -> loop (n - 1)
  in
  loop tries

let default_depth = 3

let sample ?(depth = default_depth) st ty = sample_depth st depth ty

(* An instance of a polymorphic type that keeps it inhabited may still have
   no value the generator can build (a variable instantiated by a bare mode
   outside of [v(..)], say): draw another instance, up to [inst_tries]
   times. Returns the instance with the value. *)
let inst_tries = 5

let sample_poly ?depth ?within st ty =
  let rec loop n =
    if n = 0 then None
    else
      let ty', s = instantiate ?within st ty in
      match sample ?depth st ty' with
      | Some v -> Some (s, v)
      | None -> if Ty.vars ty |> VarSet.is_empty then None else loop (n - 1)
  in
  loop inst_tries
