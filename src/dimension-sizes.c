#include "dimension-sizes.h"

#include "utils.h"

r_obj* ffi_rray_dimension_sizes(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimension_sizes(x, error_call);
}

r_obj* rray_dimension_sizes(r_obj* x, struct r_lazy error_call) {
  check_array(x, error_call);

  r_obj* dimension_sizes = r_dim(x);

  if (dimension_sizes == r_null) {
    return r_int(r_ssize_as_integer(r_length(x)));
  } else {
    return dimension_sizes;
  }
}
