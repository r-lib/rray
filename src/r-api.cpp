#include <r-api.h>

// (15 is equal to the default settings of identical())
bool r_identical(SEXP x, SEXP y) {
  return R_compute_identical(x, y, 16);
}

bool r_is_null(SEXP x) {
  return Rf_isNull(x);
}

bool r_is_missing(SEXP x) {
  return r_identical(x, R_MissingArg);
}
