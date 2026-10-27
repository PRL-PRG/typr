# Typed with the signature of strtoi in ../wrong_sig.R, which says it
# returns strings: unsound.
parse_int <- function(x) strtoi(x, 10L)

# nchar coerces its argument: values outside the domain are accepted.
count_chars <- function(x) nchar(x)

# substr coerces its argument: values outside the domain are accepted.
first_char <- function(x) substr(x, 1L, 1L)

# Refuses what is not numeric itself.
checked <- function(n) {
  if (!is.numeric(n)) stop("n must be numeric")
  n + 1
}

# Never forces y. Not exported.
first_of <- function(x, y) x
