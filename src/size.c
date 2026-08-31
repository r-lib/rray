#include "size.h"

#include "utils.h"

r_obj* ffi_rray_size(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return r_dbl((double) rray_size(ffi_x, error_call));
}

r_ssize rray_size(r_obj* x, struct r_lazy error_call) {
  check_unclassed(x, "x", error_call);
  x = arg_as_array(x, "x", error_call);
  return r_length(x);
}

r_ssize rray_size_from_dimensions(
  const int* v_dimensions,
  r_ssize dimensionality
) {
  r_ssize out = 1;

  for (r_ssize i = 0; i < dimensionality; ++i) {
    out *= v_dimensions[i];
  }

  return out;
}
