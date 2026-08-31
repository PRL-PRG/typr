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

# ---- (1) `break` / `next` inside a `for` loop -------------------------------
# Rejected with "Expression contains an orphan ret expression"; the same loop
# written with `while` is accepted, so it is the `for` lowering that is at
# fault.
for_break <- function(x) { for (i in 1:2) { break } ; x }
for_next  <- function(x) { for (i in 1:2) { next } ; x }
while_break <- function(x) { while (TRUE) { break } ; x }

# ---- (2) `<<-` onto a top-level binding ------------------------------------
# The idiom every package `.onLoad` uses. A top-level definition is immutable,
# so the assignment is rejected outright.
cache <- list()
fill_cache <- function() { cache <<- list(a = 1) ; invisible() }

# ---- (3) Elementwise functions need one overload per length class ----------
# `nchar` is length-preserving, which the algebra cannot say, so base.R
# declares it twice. The scalar overload is what makes this `INT1` rather than
# `INT`; drop it and callers that need a scalar stop type-checking.
nchar_of_scalar <- function() nchar("abc")

# ---- (4) Vector length in a type -------------------------------------------
# `c()` of three strings is a length-3 character vector; the type says only
# "some character vector", so an out-of-range index cannot be rejected.
three <- function() c("a", "b", "c")

# ---- (5) A function used before it is defined, in the same file ------------
# TypR reorders these, so this pair type-checks; mutual recursion below does
# not, no order being able to satisfy it.
uses_later <- function(x) defined_later(x)
defined_later <- function(x) x

mutual_a <- function(x) mutual_b(x)
mutual_b <- function(x) mutual_a(x)
