#include "rlang.h"
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP ffi_rray_dimensionality(SEXP x);

SEXP ffi_rray4_init_library(SEXP ns);

static const R_CallMethodDef CallEntries[] = {
  {"ffi_rray_dimensionality", (DL_FUNC) &ffi_rray_dimensionality, 1},
  {"ffi_rray4_init_library",  (DL_FUNC) &ffi_rray4_init_library, 1},
  {NULL, NULL, 0}
};

void R_init_rray4(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

SEXP ffi_rray4_init_library(SEXP ns) {
  r_init_library(ns);
  return R_NilValue;
}
