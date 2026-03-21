#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP ffi_rray_dimensionality(SEXP x);

static const R_CallMethodDef call_methods[] = {
  {"ffi_rray_dimensionality", (DL_FUNC) &ffi_rray_dimensionality, 1},
  {NULL, NULL, 0}
};

void R_init_rray4(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
