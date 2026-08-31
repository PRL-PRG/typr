# Signatures for the base R functions used by the packages of this suite.

# ========== Arithmetic and comparison ==========
## (+) : ( e1:dbl1, e2:dbl1 ) -> dbl1
## (+) : ( e1:dbl, e2:dbl ) -> dbl
## (-) : ( e1:dbl1, e2:dbl1 ) -> dbl1
## (-) : ( e1:dbl, e2:dbl ) -> dbl
## (-) : ( e1:dbl1 ) -> dbl1
## (*) : ( e1:dbl1, e2:dbl1 ) -> dbl1
## (*) : ( e1:dbl, e2:dbl ) -> dbl
## (/) : ( e1:dbl1, e2:dbl1 ) -> dbl1
## (/) : ( e1:dbl, e2:dbl ) -> dbl
## (%%) : ( e1:dbl1, e2:dbl1 ) -> dbl1
## (%/%) : ( e1:dbl1, e2:dbl1 ) -> dbl1

## (<)  : ( e1:dbl1, e2:dbl1 ) -> lgl1<>
## (<)  : ( e1:dbl, e2:dbl ) -> lgl<>
## (>)  : ( e1:dbl1, e2:dbl1 ) -> lgl1<>
## (>)  : ( e1:dbl, e2:dbl ) -> lgl<>
## (<=) : ( e1:dbl1, e2:dbl1 ) -> lgl1<>
## (<=) : ( e1:dbl, e2:dbl ) -> lgl<>
## (>=) : ( e1:dbl1, e2:dbl1 ) -> lgl1<>
## (>=) : ( e1:dbl, e2:dbl ) -> lgl<>
## (==) : ( e1:vec1, e2:vec1 ) -> lgl1<>
## (==) : ( e1:vec, e2:vec ) -> lgl<>
## (!=) : ( e1:vec1, e2:vec1 ) -> lgl1<>
## (!=) : ( e1:vec, e2:vec ) -> lgl<>

## (!) : ( x:tt ) -> ff<>
## (!) : ( x:ff ) -> tt<>
## (!) : ( x:lgl ) -> lgl<>
## (&&) : ( x:lgl1, y:lgl1 ) -> lgl1<>
## (||) : ( x:lgl1, y:lgl1 ) -> lgl1<>
## (&) : ( x:lgl, y:lgl ) -> lgl<>
## (|) : ( x:lgl, y:lgl ) -> lgl<>

## (:) : ( from:int1, to:int1 ) -> int
## (:) : ( from:dbl1, to:dbl1 ) -> dbl

# ========== Lookups ==========
## ([]) : (x:v('a), ...: dbl|CHR|lgl) -> v('a)
## ([]) : (x:{'a}, ...: dbl|CHR|lgl) -> {'a}
## ([]<-) : (x:v('a), ...: dbl|CHR, v:v('a)) -> v('a)
## ([[]]) : (x:v('a), ...: dbl|CHR) -> v1('a)
## ([[]]) : (x:{'a}, ...: dbl|CHR) -> 'a
## ($) : (x:{ #k:'a, any}, k: #k) -> 'a

# ========== Vectors and lists ==========
## c : ( ...: v('p) ) -> v('p)<>
## list : ( ...: 'a ) -> { 'a }
## list : ( ...: `r ) -> { `r }
## length : (x:any) -> INT1<>
## rev : (x:v('a)) -> v('a)
## unlist : (x:{v('a)}) -> v('a)
## seq_along : (along.with:any) -> int
## vapply : (X:v('a), FUN:@(v1('a)) -> v1('b), FUN.VALUE:v1('b)) -> v('b)
## sapply : (X:v('a), FUN:@(v1('a)) -> v1('b)) -> v('b)
## lapply : (X:v('a), FUN:@(v1('a)) -> 'b) -> {'b}

# ========== Predicates ==========
## is.null : (x:null) -> tt<>
## is.null : (x:~null) -> ff<>
## is.na : (x:vec) -> lgl<>
## is.character : (x:CHR) -> tt<>
## is.character : (x:~CHR) -> ff<>
## is.numeric : (x:DBL|INT) -> tt<>
## is.numeric : (x:~(DBL|INT)) -> ff<>
## inherits : (x:any, what:chr) -> lgl1<>

# ========== Strings ==========
## paste : (...: any, sep:chr1?, collapse:chr1?) -> CHR1
## paste0 : (...: any, collapse:chr1?) -> CHR1
## nchar : (x:chr) -> INT
## nchar : (x:chr1) -> INT1
## substring : (text:chr1, first:dbl1, last:dbl1?) -> CHR1
## substring : (text:chr, first:dbl, last:dbl?) -> CHR
## substr : (x:chr1, start:dbl1, stop:dbl1) -> CHR1
# Elementwise, so length-preserving: the scalar case needs its own overload,
# the algebra having no way to say "as long as its argument".
## toupper : (x:chr1) -> CHR1
## toupper : (x:chr) -> CHR
## tolower : (x:chr1) -> CHR1
## tolower : (x:chr) -> CHR
## sprintf : (fmt:chr1, ...: any) -> CHR1
## format : (x:any, ...: any) -> CHR
## formatC : (x:any, ...: any) -> CHR
## trimws : (x:chr) -> CHR
## strsplit : (x:chr, split:chr1, fixed:lgl1?) -> { CHR }

# ========== Regular expressions ==========
## grepl : (pattern:chr1, x:chr1, fixed:lgl1?, perl:lgl1?) -> lgl1<>
## grepl : (pattern:chr1, x:chr, fixed:lgl1?, perl:lgl1?) -> lgl<>
## sub : (pattern:chr1, replacement:chr1, x:chr1, fixed:lgl1?, perl:lgl1?) -> CHR1
## sub : (pattern:chr1, replacement:chr1, x:chr, fixed:lgl1?, perl:lgl1?) -> CHR
## gsub : (pattern:chr1, replacement:chr1, x:chr1, fixed:lgl1?, perl:lgl1?) -> CHR1
## gsub : (pattern:chr1, replacement:chr1, x:chr, fixed:lgl1?, perl:lgl1?) -> CHR
# regexpr returns the match position, carrying the match geometry as attributes.
## regexpr : (pattern:chr1, text:chr1, perl:lgl1?, fixed:lgl1?) -> INT1 with { match.length: INT1, capture.start: INT1, capture.length: INT1, any }

# ========== Maths ==========
## round : (x:dbl1, digits:dbl1?) -> dbl1
## round : (x:dbl, digits:dbl1?) -> dbl
## signif : (x:dbl1, digits:dbl1?) -> dbl1
## floor : (x:dbl1) -> dbl1
## ceiling : (x:dbl1) -> dbl1
## abs : (x:dbl1) -> dbl1
## abs : (x:dbl) -> dbl
## max : (...: dbl) -> DBL1
## min : (...: dbl) -> DBL1
## sum : (...: dbl) -> DBL1
## log : (x:dbl1, base:dbl1?) -> DBL1
## log10 : (x:dbl1) -> DBL1
## trunc : (x:dbl1) -> dbl1
## as.integer : (x:vec) -> INT
## as.numeric : (x:vec) -> DBL
## as.character : (x:vec) -> CHR

# ========== Attributes ==========
# The attribute is selected by a literal name, so the label is polymorphic.
## attr : (x:any with { #k:'b, any }, which: #k) -> 'b
## names : (x:any) -> chr

# ========== Session and environment ==========
## Sys.getenv : (x:chr1, unset:chr1?) -> CHR1
## Sys.setenv : (...: any) -> lgl
## Sys.time : () -> DBL1<Date, ...>
## getOption : (x:chr1, default:'a?) -> 'a | null
## options : (...: any) -> { any }
## l10n_info : () -> { any }
## invisible : (x:'a?) -> 'a | null
## interactive : () -> lgl1<>

# A dot-prefixed name cannot be written on the left of a `##` annotation (the
# annotation lexer's identifiers start with a letter or `_`), so the base R
# globals that have one are given a type by defining them through a helper
# whose name *is* annotatable.
## platform_ : () -> { OS.type: CHR1, file.sep: CHR1, path.sep: CHR1, GUI: CHR1, any }
platform_ <- function() list(OS.type = "unix", file.sep = "/", path.sep = ":", GUI = "X11")
.Platform <- platform_()

# ========== Control ==========
## stop : (...: any) -> empty
## stopifnot : (...: any) -> null
## warning : (...: any) -> null
## identity : (x:'a) -> 'a
# Drawing one element gives a scalar; the size being a literal, the singleton
# type of the argument is enough to say so.
## sample : (x:v('a), size:1.) -> v1('a)
## sample : (x:v('a), size:dbl1?) -> v('a)
## sample : (x:{'a}, size:dbl1?) -> {'a}
