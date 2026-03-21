#include "dimensionality.h"

SEXP ffi_rray_dimensionality(SEXP x) {
  return Rf_ScalarInteger((int) rray_dimensionality(x));
}

R_xlen_t rray_dimensionality(SEXP x) {
  SEXP dimensions = Rf_getAttrib(x, R_DimSymbol);

  if (dimensions == R_NilValue) {
    return 1;
  } else {
    return Rf_xlength(dimensions);
  }
}
