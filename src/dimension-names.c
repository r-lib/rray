#include "dimension-names.h"

#include "utils.h"

r_obj* ffi_rray_dimension_names(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimension_names(x, error_call);
}

r_obj* rray_dimension_names(r_obj* x, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim_names(x);
  FREE(1);
  return out;
}
