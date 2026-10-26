(** Type-directed generation of R values.

    [sample st ty] draws an R value of type [ty]. Generation is approximate
    and validation exact: a candidate is built by walking the structure of
    [ty] (its kinds, vector modes, list fields, classes, attributes),
    ignoring what is hard to follow (negations, intersections of atoms), and
    is kept only if its type ({!Rval.to_valty}) is a subtype of [ty]. Up to
    [tries] candidates are drawn; [None] means that none was in [ty], which
    the fuzzer reports rather than hides.

    Type variables are instantiated first ({!instantiate}), with ground types
    drawn from small pools; [sample_poly] does both.

    [gen_call] draws the arguments of a call from the domain of a function
    type, together with the result type the function must have on them. *)

open Rstt

type st = Random.State.t

let pick st = function
  | [] -> None
  | l -> Some (List.nth l (Random.State.int st (List.length l)))

let coin st = Random.State.bool st

(* ===== Type variables ===== *)

let mode_pool = [ Prim.Int.any ; Prim.Dbl.any ; Prim.Chr.any ; Prim.Lgl.any ]

(* Instances for a variable under [v(..)]: a whole vector mode ([NA]
   included: non-scalar vectors can only be typed by a whole mode, cf.
   {!Valty}). *)
let prim_pool = List.map Prim.mk mode_pool

(* Instances for a variable standing for an R value. *)
let value_pool =
  let vec m = Attr.mk_content (Vec.mk (Vec.Vector (Prim.mk m))) in
  let scalar m = Attr.mk_content (Vec.mk (Vec.Scalar (Prim.mk m))) in
  [ vec Prim.Dbl.any ; scalar Prim.Chr.any' ; Null.any ; Attr.mk_content Lst.any ]

(* Rows for row variables: no further field (a list with no other element,
   a value with no other attribute, no extra argument), or anything. *)
let row_pool = [ Row.all_fields (Ty.F.mk_descr Ty.O.absent) ; Row.any ]

(* The R values: [NULL] and symbols, and every value that may carry
   attributes. *)
let r_values = Ty.disj [ Null.any ; Sym.any ; Attr.any ]

(* The variables to instantiate: not those Rstt uses for the identity of
   polymorphic signatures. *)
let instance_vars ty = VarSet.diff (Ty.vars ty) (Arg.polymorphic_vars ())

(* Whether the variable [v] of [ty] stands for an R value: when substituting
   it with all of them changes [ty] (compared with substituting nothing).
   Otherwise it only occurs under [v(..)], where [Prim.any] absorbs the
   substitution, and it stands for a mode. *)
let stands_for_value ty v =
  let sub inst = Subst.apply (Subst.singleton1 v inst) ty in
  not (Ty.equiv (sub r_values) (sub Ty.empty))

(* A substitution of every variable of [ty], each drawn from the pool that
   fits its occurrences. Each instance is drawn among those after which
   [ty] still satisfies [ok], if any: a variable in [x: 'a & null] can only
   be [null]. *)
let instantiate ?(ok = fun _ -> true) st ty =
  let ty, s1 =
    instance_vars ty |> VarSet.elements |> List.fold_left (fun (ty, s) v ->
      let sub inst = Subst.apply (Subst.singleton1 v inst) ty in
      let pool = if stands_for_value ty v then value_pool else prim_pool in
      let pool = match List.filter (fun i -> ok (sub i)) pool with [] -> pool | p -> p in
      let inst = Option.get (pick st pool) in
      sub inst, (v, inst) :: s) (ty, [])
  in
  let ty, s2 =
    Ty.row_vars ty |> RowVarSet.elements |> List.fold_left (fun (ty, s) v ->
      let inst = Option.get (pick st row_pool) in
      Subst.apply (Subst.singleton2 v inst) ty, (v, inst) :: s) (ty, [])
  in
  ty, Subst.of_list (List.rev s1) (List.rev s2)

(* [ty] with every variable replaced by its largest instance: for a
   domain, where the variables occur positively, the union of the domains
   of all the instances. *)
let top_instance ty =
  let ty = instance_vars ty |> VarSet.elements |> List.fold_left (fun ty v ->
    let inst = if stands_for_value ty v then r_values else Prim.any in
    Subst.apply (Subst.singleton1 v inst) ty) ty in
  Ty.row_vars ty |> RowVarSet.elements |> List.fold_left (fun ty v ->
    Subst.apply (Subst.singleton2 v Row.any) ty) ty

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
                `Lang, Lang.any ; `Ext, ExternalPtr.any ; `Fun, arrows_any ]
              |> List.filter (fun (_, any) -> not (Ty.disjoint c any)) in
  Option.bind (pick st kinds) (function
    | `Vec, _ -> gen_vec st c
    | `Lst, _ -> gen_list st depth c
    | `Env, _ -> Some Rval.Env
    | `Lang, _ -> Some (Rval.Lang "f(x)")
    | `Ext, _ -> Some Rval.ExtPtr
    | `Fun, _ ->
      (* A constant function, typed at all the domains of an intersection of
         arrows, returning a value of all their codomains. *)
      let lines = Mlsem.Types.Arrow.dnf (Ty.cap c arrows_any) in
      Option.bind (pick st lines) (fun arrows ->
        let doms = List.map fst arrows in
        let codom = List.map snd arrows |> Ty.conj in
        (* With no possible result, it can only fail. *)
        if Ty.is_empty codom then Some (Rval.Fun { doms ; ret = None })
        else sample_depth st (depth - 1) codom
             |> Option.map (fun ret -> Rval.Fun { doms ; ret = Some ret })))

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

(* An instance may still have no value the generator can build: draw
   another one, up to [inst_tries] times. Returns the instance with the
   value. *)
let inst_tries = 5

let retry_instances ?ok st ty f =
  let rec loop n =
    if n = 0 then None
    else
      let ty', s = instantiate ?ok st ty in
      match f ty' with
      | Some x -> Some (s, x)
      | None -> if VarSet.is_empty (Ty.vars ty) then None else loop (n - 1)
  in
  loop inst_tries

let sample_poly ?depth st ty =
  retry_instances ~ok:(fun t -> not (Ty.is_empty t)) st ty (sample ?depth st)

(* ===== Argument lists ===== *)

(* The arguments of a call, in the order they are passed: the positional
   ones, then the named ones; and the parameters the first positional ones
   are passed to, when known. *)
type call = { pos : Rval.t list ; named : (string * Rval.t) list ; pos_names : string list }

(* As R source: a list, for the harness to [do.call]. *)
let call_to_r c =
  Rval.to_r (Rval.List (List.map (fun v -> None, v) c.pos
                        @ List.map (fun (n, v) -> Some n, v) c.named))

let absent = Ty.F.mk_descr Ty.O.absent

(* The call-site type of the arguments: exactly these, no other. *)
let call_type c =
  let f v = Ty.F.mk_descr (Ty.O.required (Rval.to_valty v).ty) in
  Arg.mk' { pos' = List.map f c.pos ; pos_tl' = absent ;
            named' = List.map (fun (n, v) -> n, f v) c.named ; named_tl' = absent }

let rec split_at n l =
  if n = 0 then [], l
  else match l with [] -> [], [] | x :: l -> let a, b = split_at (n - 1) l in x :: a, b

exception No_value

let value st ty =
  if Ty.is_empty ty then raise No_value
  else match sample st ty with Some v -> v | None -> raise No_value

(* [k] values (up to 2) for the extra arguments [...] accepts, if any. *)
let extras st f =
  let ty, _ = field f in
  if Ty.is_empty ty then [] else List.init (Random.State.int st 3) (fun _ -> value st ty)

(* Arguments for a definition-site signature. The first [i] parameters are
   passed by position, the others by name, or not at all when they are
   optional; extra positional arguments only fit when every parameter
   before [...] is passed by position (as in [Arg.mk]). With [positional]
   (the [.Call] entry points), every parameter is passed by position. *)
let call_of_def st ~positional (a : (string, Ty.F.t) Arg.atom) =
  let n = List.length a.pos_named in
  let i = if positional then n else Random.State.int st (n + 1) in
  let by_pos, by_name = split_at i a.pos_named in
  let pos = by_pos |> List.map (fun (_, f) -> value st (fst (field f))) in
  let named = by_name @ a.named |> List.filter_map (fun (name, f) ->
    let ty, opt = field f in
    if opt && (Ty.is_empty ty || coin st) then None else Some (name, value st ty)) in
  let extra_pos = if i = n && not positional then extras st a.pos_tl else [] in
  let extra_named =
    if positional then []
    else extras st a.named_tl |> List.mapi (fun k v -> Printf.sprintf "fuzz_dot%d" (k + 1), v) in
  { pos = pos @ extra_pos ; named = named @ extra_named ; pos_names = List.map fst by_pos }

(* Arguments for a call-site signature (a domain inference left as such). *)
let call_of_callsite st (a : (string, Ty.F.t) Arg.atom') =
  let present f = let ty, opt = field f in
    if opt && (Ty.is_empty ty || coin st) then None else Some (value st ty) in
  let rec prefix = function
    | [] -> []
    | f :: fs -> (match present f with None -> [] | Some v -> v :: prefix fs) in
  { pos = prefix a.pos' ;
    named = a.named' |> List.filter_map (fun (n, f) -> present f |> Option.map (fun v -> n, v)) ;
    pos_names = [] }

(* A call drawn for the function type [fty] (the type of a closure, or the
   content of one), with the type of the result it must have. *)
type sample = {
  subst : Subst.t ;           (* the instance of the type variables *)
  call : call ;
  expected : Ty.t ;           (* the result type for these arguments *)
}

let gen_call ?(positional = false) st fty =
  let content = if Ty.leq fty Attr.any then Attr.proj_content fty else fty in
  let attempt content =
    let dom = Mlsem.Types.Arrow.domain content in
    let atoms = Mlsem.Types.Arrow.dnf content |> List.concat
                |> List.concat_map (fun (d, _) -> try Arg.destruct d with Invalid_argument _ -> []) in
    let rec loop n =
      if n = 0 then None
      else match pick st atoms with
        | None -> None
        | Some atom ->
          let call =
            try Some (match atom with
                | Arg.DefSite a -> call_of_def st ~positional a
                | Arg.CallSite a -> call_of_callsite st a)
            with No_value -> None in
          match call with
          | Some call when Ty.leq (call_type call) dom ->
            Some (call, Mlsem.Types.Arrow.apply content (call_type call))
          | _ -> loop (n - 1)
    in
    loop tries
  in
  let ok t = not (Ty.is_empty (Mlsem.Types.Arrow.domain t)) in
  retry_instances ~ok st content attempt
  |> Option.map (fun (subst, (call, expected)) -> { subst ; call ; expected })
