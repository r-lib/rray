#include "dimension-sizes.h"

#include "utils.h"

r_obj* ffi_rray_dimension_sizes(r_obj* x) {
  return rray_dimension_sizes(x);
}

r_obj* rray_dimension_sizes(r_obj* x) {
  check_array(x);

  r_obj* dimension_sizes = r_dim(x);

  if (dimension_sizes == r_null) {
    return r_int(r_ssize_as_integer(r_length(x)));
  } else {
    return dimension_sizes;
  }
}
