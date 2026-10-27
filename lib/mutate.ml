(** Calls just outside the domain of a function.

    A mutant is an in-domain call ({!Gen.gen_call}) with one change: to one
    argument (its mode, its length, an [NA], its kind, its class or other
    attributes), or to the argument list (an argument dropped, an unknown
    one added). It is kept only if it lies outside the domain, of every
    instance of the type variables ({!Gen.top_instance}): a call the
    function then accepts shows that the domain is too small, and which
    change was accepted tells where.

    Outside is decided as for the results ({!Valty.mem}): on the type of
    the arguments when it is exact, and otherwise only when it is disjoint
    from the domain. *)

open Rstt

type mutant = {
  call : Gen.call ;
  arg : int option ;    (* the changed argument, by its index in [call]
                           (positional ones first); none when dropped *)
  param : string ;      (* the parameter it is passed to *)
  op : string ;         (* the change, e.g. [retype:int>dbl] *)
}

(* ===== Changes to one value ===== *)

(* The modes in the coercion order. *)
let chain = Rval.[ Raw ; Lgl ; Int ; Dbl ; Clx ; Chr ]

(* The modes next to [m] in the chain, and one other. *)
let other_modes st m =
  let i = List.find_index (( = ) m) chain |> Option.get in
  let near = List.filteri (fun j _ -> j = i - 1 || j = i + 1) chain in
  let far = List.filter (fun m' -> m' <> m && not (List.mem m' near)) chain in
  near @ Option.to_list (Gen.pick st far)

(* An element of mode [m] in place of [e]: [NA] stays [NA] where it can. *)
let convert st m e =
  let pool = Gen.elt_pool m Prim.any |> List.filter (( <> ) Rval.NA) in
  match e with
  | Rval.NA when m <> Raw -> Rval.NA
  | _ -> Option.get (Gen.pick st pool)

(* Changes that keep the length of a vector, so that attributes such as
   [names] or [dim] still fit it. *)
let content_changes st ~factor = function
  | Rval.Vec (m, es) ->
    (* R only makes factors of integer vectors. *)
    let modes = if factor then [] else other_modes st m in
    let retype = modes |> List.map (fun m' ->
      Printf.sprintf "retype:%s>%s" (Rval.mode_name m) (Rval.mode_name m'),
      Rval.Vec (m', List.map (convert st m') es)) in
    let na =
      if m = Raw || es = [] || List.mem Rval.NA es then []
      else
        let i = Random.State.int st (List.length es) in
        [ "na", Rval.Vec (m, List.mapi (fun j e -> if j = i then Rval.NA else e) es) ] in
    retype @ na
  | _ -> []

let length_changes = function
  | Rval.Vec (m, [e]) -> [ "length:0", Rval.Vec (m, []) ; "length:2", Rval.Vec (m, [e ; e]) ]
  | Rval.Vec (m, e :: _ :: _) -> [ "length:1", Rval.Vec (m, [e]) ]
  | Rval.Vec (m, []) when m <> Raw -> [ "length:1", Rval.Vec (m, [Rval.NA]) ]
  | Rval.Vec (Raw, []) -> [ "length:1", Rval.Vec (Raw, [Rval.I 0]) ]
  | _ -> []

(* A value of each kind, for a value of another one. *)
let kinds = Rval.[
  "kind:null", Null ; "kind:sym", Sym "x" ; "kind:env", Env ; "kind:lang", Lang "f(x)" ;
  "kind:fun", Fun { doms = [] ; ret = Some Null } ; "kind:list", List [] ;
  "kind:chr", Vec (Chr, [S "fuzz"]) ; "kind:dbl", Vec (Dbl, [D 1.5]) ;
]

let kind = function
  | Rval.Null -> "null" | Sym _ -> "sym" | Env -> "env" | Lang _ -> "lang"
  | Fun _ -> "fun" | List _ -> "list" | ExtPtr -> "extptr"
  | Vec (Chr, _) -> "chr" | Vec (Dbl, _) -> "dbl" | Vec _ -> "vec"
  | With _ -> "with"

let shuffle st l =
  List.map (fun x -> Random.State.bits st, x) l
  |> List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) |> List.map snd

(* Two values of other kinds: the changes that keep the kind are closer to
   the domain, and tell more about it. *)
let kind_changes st v =
  let k = match v with Rval.With { v ; _ } -> kind v | v -> kind v in
  List.filter (fun (op, _) -> op <> "kind:" ^ k) kinds |> shuffle st |> List.filteri (fun i _ -> i < 2)

let wrap_changes = function
  | Rval.List ((_, x) :: _) as v -> [ "wrap", Rval.List [None, v] ; "unwrap", x ]
  | v -> [ "wrap", Rval.List [None, v] ]

(* The value with these class and attributes: no [structure] when there
   are none. *)
let with_ v cls attrs =
  match cls, attrs with
  | None, [] -> v
  | _ -> Rval.With { v ; cls ; attrs }

let attr_changes v =
  let v, cls, attrs = match v with
    | Rval.With { v ; cls ; attrs } -> v, cls, attrs
    | v -> v, None, [] in
  match v with
  (* [NULL] and symbols carry no attributes. *)
  | Rval.Null | Sym _ -> []
  | _ ->
    let add_class = [ "class:add", with_ v (Some ("fuzz_cls" :: Option.value cls ~default:[])) attrs ] in
    let drop_class = if cls = None then [] else [ "class:drop", with_ v None attrs ] in
    let drop_attrs = attrs |> List.map (fun (n, _) ->
      "attr:drop:" ^ n, with_ v cls (List.remove_assoc n attrs)) in
    let len = match v with Rval.Vec (_, es) -> List.length es | _ -> 0 in
    let add name a =
      if len = 0 || List.mem_assoc name attrs then []
      else [ "attr:add:" ^ name, with_ v cls (attrs @ [name, a]) ] in
    let names = add "names" (Rval.Vec (Chr, List.init len (fun i -> Rval.S (Printf.sprintf "n%d" (i + 1))))) in
    let dim = add "dim" (Rval.Vec (Int, [I len ; I 1])) in
    add_class @ drop_class @ drop_attrs @ names @ dim

(* Every change to the value [v], named. *)
let value_changes st v =
  let factor = match v with
    | Rval.With { cls = Some cls ; _ } -> List.mem "factor" cls
    | _ -> false in
  let inner, rewrap = match v with
    | Rval.With { v = inner ; cls ; attrs } -> inner, (fun x -> Rval.With { v = x ; cls ; attrs })
    | v -> v, Fun.id in
  let lengths = match v with
    (* Attributes such as [names] or [dim] must fit the length. *)
    | Rval.With { attrs ; _ } when attrs <> [] -> []
    | _ -> length_changes inner in
  List.map (fun (op, x) -> op, rewrap x) (content_changes st ~factor inner @ lengths)
  @ kind_changes st v @ wrap_changes v @ attr_changes v

(* ===== Changes to a call ===== *)

let param (c : Gen.call) i =
  let n = List.length c.pos in
  if i < n then
    match List.nth_opt c.pos_names i with Some p -> p | None -> Printf.sprintf "..%d" (i + 1)
  else fst (List.nth c.named (i - n))

let replace i x l = List.mapi (fun j y -> if j = i then x else y) l
let remove i l = List.filteri (fun j _ -> j <> i) l

let candidates st (c : Gen.call) =
  let n = List.length c.pos in
  let args = List.map (fun v -> None, v) c.pos @ List.map (fun (p, v) -> Some p, v) c.named in
  let set i x =
    if i < n then { c with pos = replace i x c.pos }
    else { c with named = replace (i - n) (fst (List.nth c.named (i - n)), x) c.named } in
  let drop i =
    (* The positional arguments after [i] move to the parameters before. *)
    if i < n then { c with pos = remove i c.pos ; pos_names = List.filteri (fun j _ -> j < n - 1) c.pos_names }
    else { c with named = remove (i - n) c.named } in
  let changed = args |> List.mapi (fun i (_, v) ->
    value_changes st v |> List.map (fun (op, x) ->
      { call = set i x ; arg = Some i ; param = param c i ; op })) |> List.concat in
  let dropped = args |> List.mapi (fun i _ ->
    { call = drop i ; arg = None ; param = param c i ; op = "drop" }) in
  let extra = { call = { c with named = c.named @ ["fuzz_extra", Rval.Vec (Dbl, [D 1.])] } ;
                arg = Some (List.length args) ; param = "fuzz_extra" ; op = "extra" } in
  changed @ dropped @ [extra]

(* Whether the arguments of [c] lie outside [dom]. *)
let outside dom (c : Gen.call) =
  let exact = List.for_all (fun v -> (Rval.to_valty v).exact) (c.pos @ List.map snd c.named) in
  Valty.mem { ty = Gen.call_type c ; exact } dom = Out

(* Up to [n] mutants of the call [c] to a function of type [fty] (as for
   {!Gen.gen_call}), each with a different change, in random order. *)
let mutants ?(n = max_int) st fty (c : Gen.call) =
  let content = if Ty.leq fty Attr.any then Attr.proj_content fty else fty in
  let dom = Mlsem.Types.Arrow.domain (Gen.top_instance content) in
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | m :: l -> if outside dom m.call then m :: take (n - 1) l else take n l in
  take n (shuffle st (candidates st c))
