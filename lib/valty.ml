(** The type of a value observed at run time.

    The fuzzing harness runs package functions in R and reports each result as
    a structural description, in JSON. This module turns such a description
    into the most precise type the Rstt encoding can express, so that "the
    value belongs to [t]" is decided by subtyping against [t].

    The description of a value is an object whose ["t"] field gives its kind:
    - ["null"], ["env"], ["sym"], ["lang"], ["pairlist"], ["extptr"],
      ["closure"], ["builtin"]: no further field; ["other"] for the remaining
      R types (S4 objects, [...] objects, ...), with their [typeof] in
      ["type"]. Pairlists are typed as calls, as NativeSem does ([LISTSXP] in
      its [base.ty]);
    - ["vec"]: ["mode"] (one of [lgl], [int], [dbl], [clx], [chr], [raw]) and
      ["len"]. A vector of length 1 also has ["na"], and when it is not [NA]
      its element in ["x"]: an integer for [int], and for an integer-valued
      [dbl] that fits an OCaml [int]; the string ["_"] for any other [dbl]
      (non-integer, [NaN], infinite); a string for [chr]; a boolean for [lgl].
      The element of a [clx] or [raw] scalar is not needed;
    - ["list"]: ["elts"], the descriptions of the elements in order, and
      ["names"], their names ([null] when the list has none, [""] for an
      unnamed element).
    Every kind but ["null"] may also carry ["class"], the class attribute as
    a list of strings, and ["attrs"], an object mapping each other attribute
    to the description of its value.

    The encoding knows the mode of a vector, whether it is a scalar, and for a
    scalar, its value. A longer vector has the type of all the vectors of its
    mode and length other than 1, which is exact: no type tells two of them
    apart. Some values have types that over-approximate them, and a failed
    subtyping check on them proves nothing. The result says so in [exact]:
    - closures and builtins, typed as any function, and other values, typed
      as [any];
    - lists with unnamed elements: all of them go to the list's tail, which
      does not count them, so [list(1)] gets the type of [list()] as well;
    - lists with duplicated names, of which the encoding keeps one;
    - doubles too large for an OCaml [int], typed as any double. *)

open Rstt

type t = { ty : Ty.t ; exact : bool }

exception Bad_description of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad_description s)) fmt

(* ===== JSON access ===== *)

let field k = function
  | `Assoc l -> (try List.assoc k l with Not_found -> `Null)
  | _ -> bad "an object was expected"

let to_string k = function
  | `String s -> s
  | _ -> bad "field %s: a string was expected" k

let to_int k = function
  | `Int i -> i
  | _ -> bad "field %s: an integer was expected" k

let to_list k = function
  | `List l -> l
  | `Null -> []
  | _ -> bad "field %s: a list was expected" k

(* ===== Vectors ===== *)

(* Each mode, with and without [NA]. *)
let mode_any = function
  | "lgl" -> Prim.Lgl.any, Prim.Lgl.any'
  | "int" -> Prim.Int.any, Prim.Int.any'
  | "dbl" -> Prim.Dbl.any, Prim.Dbl.any'
  | "clx" -> Prim.Clx.any, Prim.Clx.any'
  | "chr" -> Prim.Chr.any, Prim.Chr.any'
  | "raw" -> Prim.Raw.any, Prim.Raw.any'
  | m -> bad "unknown vector mode %s" m

(* The non-integer doubles, [NaN] and the infinities: Rstt only singles out
   the integer-valued doubles. *)
let dbl_nonint = Ty.diff Prim.Dbl.any' (Prim.Dbl.interval' (None, None))

(* The element type of a scalar, and whether it is exact. *)
let scalar_prim mode d =
  let any, any' = mode_any mode in
  match field "na" d with
  | `Bool true ->
    if mode = "raw" then bad "a raw vector cannot hold NA" ;
    Ty.diff any any', true
  | _ ->
    let x = field "x" d in
    match mode, x with
    | "int", _ -> Prim.Int.int' (to_int "x" x), true
    | "dbl", `Int i -> Prim.Dbl.int' i, true
    (* JSON writers may print a large integer-valued double in exponent form. *)
    | "dbl", `Float f when Float.is_integer f && Float.abs f < 9007199254740992. ->
      Prim.Dbl.int' (int_of_float f), true
    | "dbl", `String "_" -> dbl_nonint, true
    | "dbl", `String "big" -> any', false
    | "chr", `String s -> Prim.Chr.str' s, true
    | "lgl", `Bool b -> Prim.Lgl.bool' b, true
    | ("clx" | "raw"), _ -> any', true
    | _ -> bad "invalid element for a %s scalar" mode

let vec d =
  let mode = field "mode" d |> to_string "mode" in
  match field "len" d |> to_int "len" with
  | 1 ->
    let p, exact = scalar_prim mode d in
    Vec.mk (Vec.Scalar (Prim.mk p)), exact
  | _ ->
    (* Non-scalar vectors can only be typed by their whole mode, [NA]
       included (cf. [Vec.mk]). *)
    let any, _ = mode_any mode in
    Vec.mk_line (Vec.Vector (Prim.mk any), [Vec.Scalar Ty.any]), true

(* ===== Values ===== *)

let field_of ty = Ty.O.required ty |> Ty.F.mk_descr
let absent = Ty.F.mk_descr Ty.O.absent

let classes d =
  match field "class" d with
  | `Null -> Classes.noclass
  | cls ->
    let pos =
      to_list "class" cls |> List.map (fun c -> Classes.L (to_string "class" c, []))
    in
    Classes.mk { pos ; neg = [] ; unk = [] ; tail = Classes.NoOther }

let rec of_json d =
  match field "t" d |> to_string "t" with
  | "null" -> { ty = Null.any ; exact = true }
  | kind ->
    let content, exact =
      match kind with
      | "vec" -> vec d
      | "list" -> list d
      | "env" -> Env.any, true
      | "sym" -> Sym.any, true
      | "lang" | "pairlist" -> Lang.any, true
      | "extptr" -> ExternalPtr.any, true
      | "closure" | "builtin" -> Arrows.any |> Descr.mk_arrows |> Ty.mk_descr, false
      | "other" -> Ty.any, false
      | k -> bad "unknown value kind %s" k
    in
    let attrs, attrs_exact = named_fields (field "attrs" d) in
    let ty = Attr.mk { content ; classes = classes d ; attrs } in
    { ty ; exact = exact && attrs_exact }

(* The attributes other than the class, as a list type with no other field. *)
and named_fields = function
  | `Null -> Lst.empty, true
  | `Assoc l ->
    let bindings = l |> List.map (fun (k, v) -> k, of_json v) in
    let ty = Lst.mk { bindings = List.map (fun (k, v) -> k, field_of v.ty) bindings ;
                      tl = absent } in
    ty, List.for_all (fun (_, v) -> v.exact) bindings
  | _ -> bad "field attrs: an object was expected"

and list d =
  let elts = field "elts" d |> to_list "elts" |> List.map of_json in
  let names =
    match field "names" d with
    | `Null -> List.map (fun _ -> "") elts
    | names -> to_list "names" names |> List.map (to_string "names")
  in
  if List.length names <> List.length elts then bad "names and elements differ in number" ;
  let elts = List.combine names elts in
  let named, unnamed = List.partition (fun (n, _) -> n <> "") elts in
  let tl =
    match unnamed with
    | [] -> absent
    | _ -> List.map (fun (_, v) -> v.ty) unnamed |> Ty.disj |> Ty.O.optional |> Ty.F.mk_descr
  in
  let distinct = List.sort_uniq String.compare (List.map fst named) in
  let ty = Lst.mk { bindings = List.map (fun (n, v) -> n, field_of v.ty) named ; tl } in
  let exact =
    unnamed = [] && List.length distinct = List.length named
    && List.for_all (fun (_, v) -> v.exact) elts
  in
  ty, exact

(* ===== Membership ===== *)

type verdict = In | Out | Unknown

(* Whether the value described by [v] belongs to [ty]. Subtyping is complete
   only on exact types: an over-approximation that is not a subtype may still
   describe a value of [ty], unless it is disjoint from it. *)
let mem v ty =
  if Ty.leq v.ty ty then In
  else if v.exact || Ty.disjoint v.ty ty then Out
  else Unknown
