(* Types of observed values: each case is a value description, as the harness
   writes it, checked for membership against a few types. *)

open Mlsem.Types

let cases = [
  "3L", {|{"t":"vec","mode":"int","len":1,"na":false,"x":3}|},
  ["int1"; "^int1"; "3L"; "4L"; "dbl1"; "DBL1"; "chr1"; "int"; "null"] ;
  "NA_integer_", {|{"t":"vec","mode":"int","len":1,"na":true}|},
  ["int1"; "^int1"; "INT1"] ;
  "1:3", {|{"t":"vec","mode":"int","len":3}|},
  ["int"; "INT"; "int1"; "dbl"; "lgl"] ;
  "integer(0)", {|{"t":"vec","mode":"int","len":0}|},
  ["int"; "int1"; "null"] ;
  "0.5", {|{"t":"vec","mode":"dbl","len":1,"na":false,"x":"_"}|},
  ["dbl1"; "^dbl1"; "42."; "int1"] ;
  "42", {|{"t":"vec","mode":"dbl","len":1,"na":false,"x":42}|},
  ["42."; "42L"; "^dbl1"] ;
  "\"a\"", {|{"t":"vec","mode":"chr","len":1,"na":false,"x":"a"}|},
  ["\"a\""; "\"b\""; "chr1"; "^chr1"; "dbl1"] ;
  "TRUE", {|{"t":"vec","mode":"lgl","len":1,"na":false,"x":true}|},
  ["tt"; "ff"; "lgl1"; "int1"] ;
  "NULL", {|{"t":"null"}|},
  ["null"; "any"; "int"] ;
  "factor(\"a\")",
  {|{"t":"vec","mode":"int","len":1,"na":false,"x":1,"class":["factor"],
     "attrs":{"levels":{"t":"vec","mode":"chr","len":1,"na":false,"x":"a"}}}|},
  ["INT<factor>"; "INT<>"; "int"; "int1 with { levels: chr }"] ;
  "c(a = 1, b = 2)",
  {|{"t":"vec","mode":"dbl","len":2,
     "attrs":{"names":{"t":"vec","mode":"chr","len":2}}}|},
  ["dbl with { names: chr }"; "dbl with { }"; "dbl"] ;
  "list(a = 1L)",
  {|{"t":"list","elts":[{"t":"vec","mode":"int","len":1,"na":false,"x":1}],"names":["a"],
     "attrs":{"names":{"t":"vec","mode":"chr","len":1,"na":false,"x":"a"}}}|},
  ["{ a: int1 }"; "{ b: int1 }"; "{ a: chr1 }"; "list"] ;
  "list(1L)",
  {|{"t":"list","elts":[{"t":"vec","mode":"int","len":1,"na":false,"x":1}]}|},
  ["{ int1 }"; "{ chr1 }"; "{ }"] ;
  "function(x) x", {|{"t":"closure"}|},
  ["any"; "(x: any) -> any"; "int"] ;
  "as.name(\"x\")", {|{"t":"sym"}|},
  ["sym"; "lang"; "any"] ;
  "new.env()", {|{"t":"env"}|},
  ["env"; "null"] ;
  "pairlist(a = 1)",
  {|{"t":"pairlist","attrs":{"names":{"t":"vec","mode":"chr","len":1,"na":false,"x":"a"}}}|},
  ["lang"; "list"; "null"] ;
]

let verdict = function Typr.Valty.In -> "in" | Out -> "out" | Unknown -> "unknown"

let run () =
  cases |> List.iter (fun (label, desc, tys) ->
    let v = Typr.Valty.of_json (Yojson.Safe.from_string desc) in
    Printf.printf "%s : %s%s\n" label
      (Typr.Report.one_line Ty.pp v.ty) (if v.exact then "" else "  (inexact)") ;
    tys |> List.iter (fun s ->
      let ty = Lang.Defs.ty s in
      Printf.printf "  %-26s %s\n" s (verdict (Typr.Valty.mem v ty))))

let () =
  Lang.Driver.setup () ;
  PEnv.sequential_handler PEnv.empty run () |> ignore
