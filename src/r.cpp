#include <api.h>

// (15 is equal to the default settings of identical())
bool r_identical(SEXP x, SEXP y) {
  return R_compute_identical(x, y, 15);
}

bool r_is_null(SEXP x) {
  return Rf_isNull(x);
}
