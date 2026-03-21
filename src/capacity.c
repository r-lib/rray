#include "capacity.h"

#include "utils.h"

r_obj* ffi_rray_capacity(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return r_dbl((double) rray_capacity(x, error_call));
}

r_ssize rray_capacity(r_obj* x, struct r_lazy error_call) {
  check_array(x, error_call);
  return r_length(x);
}

R_xlen_t rray_capacity_from_dimension_sizes(
  const int* v_dimension_sizes,
  R_xlen_t dimensionality
) {
  R_xlen_t out = 1;

  for (R_xlen_t i = 0; i < dimensionality; ++i) {
    out *= v_dimension_sizes[i];
  }

  return out;
}
