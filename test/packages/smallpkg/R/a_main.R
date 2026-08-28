pair_of <- function(x, y) {
  .Call(C_make_pair, tag(x), tag(y))
}

first_of <- function(x, y) {
  .Call(C_first, pair_of(x, y))
}

# `helper` is defined after the function that uses it: the pipeline has to
# reorder them, since Rsem checks definitions in the order it is given them.
via_helper <- function(x) helper(x)

helper <- function(x) .Call(C_first, x)
