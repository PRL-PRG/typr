(** R values on the OCaml side.

    The fuzzer builds the arguments it passes to package functions as values
    of this small AST, prints them as R source for the harness to evaluate, and
    types them with {!Valty}, through [describe]: the description R itself
    would give of the evaluated value ([fuzz/describe.R]). The two must agree,
    which the [@fuzz-r] tests check. *)

type mode = Lgl | Int | Dbl | Clx | Chr | Raw

type elt =
  | NA
  | B of bool
  | I of int             (* an [int] or a [raw] element *)
  | D of float
  | C of float * float
  | S of string

type t =
  | Null
  | Vec of mode * elt list
  | List of (string option * t) list
  | Env
  | Sym of string
  | Lang of string                       (* R source of a call *)
  | Fun of t                             (* [function(...) v] *)
  | With of { v : t ; cls : string list option ; attrs : (string * t) list }

let mode_name = function
  | Lgl -> "lgl" | Int -> "int" | Dbl -> "dbl" | Clx -> "clx" | Chr -> "chr" | Raw -> "raw"

(* ===== R source ===== *)

(* An R string literal. Non-ASCII bytes go through as they are: the source
   is UTF-8, and so is the harness's locale. *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"' ;
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | '\r' -> Buffer.add_string b "\\r"
    | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
    | c -> Buffer.add_char b c) s ;
  Buffer.add_char b '"' ;
  Buffer.contents b

let double f =
  if Float.is_nan f then "NaN"
  else if f = Float.infinity then "Inf"
  else if f = Float.neg_infinity then "-Inf"
  else Printf.sprintf "%.17g" f

let elt_to_r mode e =
  match mode, e with
  | Lgl, NA -> "NA"
  | Int, NA -> "NA_integer_"
  | Dbl, NA -> "NA_real_"
  | Clx, NA -> "NA_complex_"
  | Chr, NA -> "NA_character_"
  | Lgl, B b -> if b then "TRUE" else "FALSE"
  | Int, I i -> Printf.sprintf "%dL" i
  | Dbl, D f -> double f
  | Clx, C (re, im) -> Printf.sprintf "complex(real = %s, imaginary = %s)" (double re) (double im)
  | Chr, S s -> quote s
  | Raw, I i -> string_of_int i
  | _ -> invalid_arg "Rval: element of the wrong mode"

let empty_vec = function
  | Lgl -> "logical(0)" | Int -> "integer(0)" | Dbl -> "numeric(0)"
  | Clx -> "complex(0)" | Chr -> "character(0)" | Raw -> "raw(0)"

let arg_name n = quote n ^ " = "

let rec to_r = function
  | Null -> "NULL"
  | Vec (m, []) -> empty_vec m
  | Vec (Raw, es) -> Printf.sprintf "as.raw(c(%s))" (List.map (elt_to_r Raw) es |> String.concat ", ")
  | Vec (m, [e]) -> elt_to_r m e
  | Vec (m, es) -> Printf.sprintf "c(%s)" (List.map (elt_to_r m) es |> String.concat ", ")
  | List l ->
    let elt (n, v) = (match n with Some n -> arg_name n | None -> "") ^ to_r v in
    Printf.sprintf "list(%s)" (List.map elt l |> String.concat ", ")
  | Env -> "new.env()"
  | Sym s -> Printf.sprintf "as.name(%s)" (quote s)
  | Lang s -> Printf.sprintf "quote(%s)" s
  | Fun v -> Printf.sprintf "function(...) %s" (to_r v)
  | With { v ; cls ; attrs } ->
    let attrs = attrs |> List.map (fun (n, a) -> arg_name n ^ to_r a) in
    let cls = match cls with
      | None -> []
      | Some cs -> ["class = " ^ to_r (Vec (Chr, List.map (fun c -> S c) cs))]
    in
    Printf.sprintf "structure(%s)" (to_r v :: attrs @ cls |> String.concat ", ")

(* ===== Description, as [fuzz/describe.R] gives it ===== *)

(* Doubles that are integers below 2^53 in absolute value are exactly
   representable, and fit an OCaml [int]. *)
let int_limit = 9007199254740992.

let elt_json mode e : (string * Yojson.Safe.t) list =
  match mode, e with
  | _, NA -> ["na", `Bool true]
  | Int, I i -> ["na", `Bool false ; "x", `Int i]
  | Dbl, D f ->
    let x =
      if Float.is_integer f && Float.abs f < int_limit then `Int (int_of_float f)
      else if Float.is_integer f then `String "big"
      else `String "_"
    in
    ["na", `Bool false ; "x", x]
  | Chr, S s -> ["na", `Bool false ; "x", `String s]
  | Lgl, B b -> ["na", `Bool false ; "x", `Bool b]
  | (Clx | Raw), _ -> ["na", `Bool false]
  | _ -> invalid_arg "Rval: element of the wrong mode"

let chr_vec l = Vec (Chr, List.map (fun s -> S s) l)

let rec describe v : Yojson.Safe.t =
  match v with
  | Null -> `Assoc ["t", `String "null"]
  | Vec (m, es) ->
    let scalar = match es with [e] -> elt_json m e | _ -> [] in
    `Assoc (["t", `String "vec" ; "mode", `String (mode_name m) ;
             "len", `Int (List.length es)] @ scalar)
  | List l ->
    let named = List.exists (fun (n, _) -> n <> None) l in
    let names = List.map (fun (n, _) -> Option.value n ~default:"") l in
    let base =
      ["t", `String "list" ; "elts", `List (List.map (fun (_, v) -> describe v) l)]
      @ (if named then ["names", `List (List.map (fun n -> `String n) names)] else [])
    in
    `Assoc (base @ (if named then ["attrs", `Assoc ["names", describe (chr_vec names)]] else []))
  | Env -> `Assoc ["t", `String "env"]
  | Sym _ -> `Assoc ["t", `String "sym"]
  | Lang _ -> `Assoc ["t", `String "lang"]
  | Fun _ -> `Assoc ["t", `String "closure"]
  | With { v ; cls ; attrs } ->
    let fields = match describe v with `Assoc f -> f | _ -> assert false in
    let inner_attrs = match List.assoc_opt "attrs" fields with
      | Some (`Assoc a) -> a | _ -> [] in
    let fields = List.remove_assoc "attrs" fields in
    let attrs = inner_attrs @ List.map (fun (n, a) -> n, describe a) attrs in
    let cls = match cls with
      | None -> [] | Some cs -> ["class", `List (List.map (fun c -> `String c) cs)] in
    `Assoc (fields @ cls @ (if attrs = [] then [] else ["attrs", `Assoc attrs]))

let to_valty v = Valty.of_json (describe v)
