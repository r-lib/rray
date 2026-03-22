#include <R_ext/Rdynload.h>
#include <stdlib.h>

#include "rlang.h"

extern r_obj* ffi_rray_broadcast(
  r_obj* x,
  r_obj* dimension_sizes,
  r_obj* frame
);
extern r_obj* ffi_rray_capacity(r_obj* x, r_obj* frame);
extern r_obj* ffi_rray_dimensionality(r_obj* x, r_obj* frame);
extern r_obj* ffi_rray_dimension_sizes(r_obj* x, r_obj* frame);
extern r_obj* ffi_rray_dimension_names(r_obj* x, r_obj* frame);

r_obj* ffi_rray4_init_library(r_obj* ns);

static const R_CallMethodDef CallEntries[] = {
  { "ffi_rray_broadcast", (DL_FUNC) &ffi_rray_broadcast, 3 },
  { "ffi_rray_capacity", (DL_FUNC) &ffi_rray_capacity, 2 },
  { "ffi_rray_dimension_sizes", (DL_FUNC) &ffi_rray_dimension_sizes, 2 },
  { "ffi_rray_dimensionality", (DL_FUNC) &ffi_rray_dimensionality, 2 },
  { "ffi_rray_dimension_names", (DL_FUNC) &ffi_rray_dimension_names, 2 },
  { "ffi_rray4_init_library", (DL_FUNC) &ffi_rray4_init_library, 1 },
  { NULL, NULL, 0 }
};

void R_init_rray4(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

r_obj* ffi_rray4_init_library(r_obj* ns) {
  r_init_library(ns);
  return r_null;
}
