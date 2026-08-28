#include <Rinternals.h>

SEXP make_pair(SEXP x, SEXP y) {
  return Rf_lang2(x, y);
}

SEXP first(SEXP p) {
  return CAR(p);
}
