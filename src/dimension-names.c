#include "dimension-names.h"

#include "dimensionality.h"
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

  const r_ssize dimensionality = rray_dimensionality(x, error_call);
  out = KEEP(r_alloc_list(dimensionality));

  // If we have a bare vector, use its `names` if they exist,
  // otherwise just return the list of `NULL`
  if (dimensionality == 1 && r_dim(x) == r_null) {
    r_obj* names = r_names(x);
    if (names != r_null) {
      r_list_poke(out, 0, names);
    }
  }

  FREE(1);
  return out;
}
