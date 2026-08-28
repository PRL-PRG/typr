pair_of <- function(x, y) {
  .Call(C_make_pair, tag(x), tag(y))
}

first_of <- function(x, y) {
  .Call(C_first, pair_of(x, y))
}
