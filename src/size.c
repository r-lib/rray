#include "size.h"

#include "utils.h"

r_obj* ffi_rray_size(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return r_dbl((double) rray_size(ffi_x, rray_args.x, error_call));
}

r_ssize rray_size(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  check_unclassed(x, arg, error_call);
  x = arg_as_array(x, arg, error_call);
  return r_length(x);
}

r_ssize rray_size_from_dimensions(const int* v_dimensions, int dimensionality) {
  r_ssize out = 1;

  for (int i = 0; i < dimensionality; ++i) {
    out *= v_dimensions[i];
  }

  return out;
}
