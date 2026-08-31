# ---- (0) NA and NaN --------------------------------------------------------
# Fixed. R has one missing value per mode, and each now types in that mode;
# `NaN` is a double, as `typeof(NaN)` says.
na_lgl  <- NA
na_int  <- NA_integer_
na_real <- NA_real_
na_cplx <- NA_complex_
na_chr  <- NA_character_
nan     <- NaN

# ---- (0b) Forwarding `...` to an overloaded function ------------------------
# Fixed. The inferred type used to be unprintable: a def-site argument is
# represented as the union of the ways its parameters split between positional
# and named, and intersecting it here removes the fully-positional atom the
# printer recovers the signature from. The remaining atoms are now read as
# call-sites instead of dropped.
forward_dots <- function(x, ...) sub("a", "b", x, ...)

# ---- (0c) Prose comments collided with annotations --------------------------
# Fixed. The annotation marker used to be `##`, which is also a common way of
# writing an ordinary comment in R, so every such line was parsed as a
# declaration and reported as a syntax error. It is now `#|`: the prose below
# is passed over in silence, and only the `#|` line annotates. Were it not
# read, the inferred type would be the polymorphic `(x: 'a) -> 'a`.
## Twice the input -- prose, not a declaration.
#| annotated : (x: dbl) -> dbl
annotated <- function(x) x

# ---- (0d) Dot-prefixed names could not be annotated -------------------------
# Fixed. The annotation lexer's identifiers started with a letter or `_`, so
# `.Platform` and friends -- exactly the names a prelude needs -- could not be
# written on the left of an annotation. base.R used to launder them through a
# helper whose name was annotatable; it declares them directly now, and a
# package's own dot-named definitions can be annotated too. Were the annotation
# below not read, `.dot_helper` would infer the polymorphic `(x: 'a) -> 'a`.
#| .dot_helper : (x: dbl) -> dbl
.dot_helper <- function(x) x

uses_dot_helper <- function() .dot_helper(1)
uses_dot_global <- function() .Platform$OS.type
uses_dot_machine <- function() .Machine$double.eps

# ---- (0e) Operator names and dot-initial parameter labels -------------------
# Fixed, in two separate lexers. Rsem's annotation lexer left `^` and `~` out
# of the alphabet an operator name may use, so `#| (^) : ...` did not lex;
# RSTT's *type* lexer rejected a label starting with a dot, so a signature
# could not name R's conventional `.Data` / `.x` / `.f` parameters. base.R can
# now declare `(^)` and `structure`, both of which it was missing.
powered <- function(x) x^2
structured <- function(x) structure(x, class = "foo")

#| dot_labelled : (.Data: dbl, .f: chr) -> dbl
dot_labelled <- function(.Data, .f) .Data
uses_dot_labelled <- function() dot_labelled(.Data = 1, .f = "a")

# ---- (1) `break` / `next` inside a `for` loop -------------------------------
# Fixed. The loop was rejected with "Expression contains an orphan ret
# expression" while the same loop written with `while` was accepted: MLsem's
# lowering of `while` wraps the body in a BLoop block, and Rsem's `for` did
# not, so the jump had no block to return to. The body -- the loop variable's
# assignment included -- now gets one.
for_break <- function(x) { for (i in 1:2) { break } ; x }
for_next  <- function(x) { for (i in 1:2) { next } ; x }
while_break <- function(x) { while (TRUE) { break } ; x }
for_break_cond <- function() { s <- 0 ; for (i in 1:9) { if (i > 3) break ; s <- i } ; s }
for_nested <- function() { s <- 0 ; for (i in 1:3) { for (j in 1:3) break ; s <- i } ; s }

# ---- (2) `<<-` onto a top-level binding ------------------------------------
# Resolved by annotation. A top-level definition is immutable, so the
# superassignment is rejected outright...
cache <- list()
fill_cache <- function() { cache <<- list(a = 1) ; invisible() }

# ...but a *value* annotation makes the binding `AnnotMut`, and then it is
# accepted. This is the idiom every package `.onLoad` uses, and it needed the
# dot-name fix of (0d) as much as this one.
#| acache : { a: dbl1 } | { }
acache <- list()
fill_acache <- function() { acache <<- list(a = 1) ; invisible() }

#| .symbols : { tick: CHR1 } | { }
.symbols <- list()
#| .onLoad : (libname: CHR1, pkgname: CHR1) -> null
.onLoad <- function(libname, pkgname) { .symbols <<- list(tick = "v") ; NULL }

# ---- (2b) An assignment inside a top-level expression -- fixed -------------
# The same "immutable variable" message used to come from a second, unrelated
# cause: `Scope.new_scope` is opened for a function and nothing else, so an
# assignment nested in a top-level *expression* targeted a top-level name,
# which is immutable unless declared. Two fixes, for the two shapes.
#
# A brace is not a scope in R, so these really are top-level definitions and
# they are now created as such -- assigned once, no declaration needed.
blk <- { u <- 1 ; u }
use_u <- function() u

if (interactive()) { z <- 1 } else { z <- 2 }
use_z <- function() z

# A `for` at top level, whose loop variable persists afterwards as it does in R.
for (i in 1:2) { w <- i }
use_w <- function() w
use_i <- function() i

# A declared name keeps the type its declaration gives it, and stays mutable.
#| au : dbl
ablk <- { au <- 1 ; au }

# `local()`, on the other hand, *does* evaluate in a fresh environment, so its
# names are not top-level at all. It gets a scope instead -- which is what
# `prettyunits::format_time_ago` needs, and what lets an inner function be
# annotated, since the name is then an ordinary scoped local.
fblk <- local({
  #| afn : (a:dbl1) -> dbl1
  afn <- function(a) a + 1
  list(h = afn)
})
use_fblk <- function() fblk$h(1)

# `local` + `<<-`, the counter idiom (ggplot2 builds its ids this way): the
# scope makes `n` a mutable local of the block, so the closure can assign it.
counter <- local({ n <- 0 ; function() { n <<- n + 1 ; n } })

# What a `local` block binds must not escape it.
leaked <- function() afn

# ---- (3) Elementwise functions need one overload per length class ----------
# `nchar` is length-preserving, which the algebra cannot say, so base.R
# declares it twice. The scalar overload is what makes this `INT1` rather than
# `INT`; drop it and callers that need a scalar stop type-checking.
nchar_of_scalar <- function() nchar("abc")

# ---- (4) Vector length in a type -------------------------------------------
# `c()` of three strings is a length-3 character vector; the type says only
# "some character vector", so an out-of-range index cannot be rejected.
three <- function() c("a", "b", "c")

# ---- (4b) An unbound name reported against an unrelated call ---------------
# Alone, an unbound callee is reported as such. Put a successful application
# before it and the error moves onto that application instead, with the
# parameter shown unrefined -- the unbound name is never mentioned. The same
# body types when the second callee is bound.
unbound_alone <- function(x) undefined_fn(x)
unbound_after_call <- function(x) { y <- nchar(x) ; undefined_fn(y) }
bound_after_call <- function(x) { y <- nchar(x) ; length(y) }

# ---- (5) A function used before it is defined, in the same file ------------
# TypR reorders these, so this pair type-checks; mutual recursion below does
# not, no order being able to satisfy it.
uses_later <- function(x) defined_later(x)
defined_later <- function(x) x

mutual_a <- function(x) mutual_b(x)
mutual_b <- function(x) mutual_a(x)
