# The structural description of an R value that TypR's Valty module types
# (lib/valty.ml documents the format). Shared by the fuzzing harness and the
# tests that check it against the OCaml side (lib/rval.ml).

fuzz_modes <- c(logical = "lgl", integer = "int", double = "dbl",
                complex = "clx", character = "chr", raw = "raw")

fuzz_describe <- function(x) {
  if (is.null(x)) return(list(t = "null"))
  type <- typeof(x)
  d <- switch(type,
    logical = , integer = , double = , complex = , character = , raw =
      fuzz_describe_vec(x, fuzz_modes[[type]]),
    list = fuzz_describe_list(x),
    environment = list(t = "env"),
    symbol = list(t = "sym"),
    language = list(t = "lang"),
    pairlist = list(t = "pairlist"),
    externalptr = list(t = "extptr"),
    closure = list(t = "closure"),
    special = , builtin = list(t = "builtin"),
    list(t = "other", type = type))
  a <- attributes(x)
  if (!is.null(a$class)) d$class <- as.list(a$class)
  a$class <- NULL
  if (length(a)) d$attrs <- lapply(a, fuzz_describe)
  d
}

fuzz_describe_vec <- function(x, mode) {
  d <- list(t = "vec", mode = mode, len = length(x))
  if (length(x) != 1L) return(d)
  # NaN is no NA for TypR: it is one of the non-integer doubles.
  na <- if (mode == "raw") FALSE else is.na(x) && !is.nan(x)
  d$na <- na
  if (na) return(d)
  x <- unclass(x)
  attributes(x) <- NULL
  d$x <- switch(mode,
    int = x,
    dbl = if (is.finite(x) && x == trunc(x)) {
            if (abs(x) < 2^53) x else "big"
          } else "_",
    chr = enc2utf8(x),
    lgl = x,
    NULL)
  d
}

fuzz_describe_list <- function(x) {
  elts <- lapply(seq_along(x), function(i) fuzz_describe(x[[i]]))
  d <- list(t = "list", elts = elts)
  if (!is.null(names(x))) d$names <- as.list(names(x))
  d
}

# One description per line.
fuzz_describe_json <- function(x)
  jsonlite::toJSON(fuzz_describe(x), auto_unbox = TRUE, null = "null", digits = NA)
