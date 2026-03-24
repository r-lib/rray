#include "dimensionality.h"

#include "dimension-sizes.h"

r_obj* ffi_rray_dimensionality(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return r_int((int) rray_dimensionality(x, error_call));
}

r_ssize rray_dimensionality(r_obj* x, struct r_lazy error_call) {
  r_obj* dimension_sizes = rray_dimension_sizes(x, error_call);
  return rray_dimensionality_from_dimension_sizes(dimension_sizes);
}

r_ssize rray_dimensionality_from_dimension_sizes(r_obj* dimension_sizes) {
  return r_length(dimension_sizes);
}
