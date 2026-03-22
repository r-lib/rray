#include "dimension-names.h"

#include "utils.h"

r_obj* ffi_rray_dimension_names(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimension_names(x, error_call);
}

r_obj* rray_dimension_names(r_obj* x, struct r_lazy error_call) {
  check_array(x, error_call);

  // Use `dimnames` as is if they exist
  r_obj* out = r_dim_names(x);
  if (out != r_null) {
    return out;
  }

  // Bare vectors with `names` result in a 1 element list
  if (r_dim(x) == r_null) {
    r_obj* names = r_names(x);

    if (names != r_null) {
      KEEP(names);
      r_obj* out = KEEP(r_alloc_list(1));
      r_list_poke(out, 0, names);
      FREE(2);
      return out;
    }
  }

  // No dimension names at all
  return r_null;
}
