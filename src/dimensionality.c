#include "dimensionality.h"
#include "utils.h"

r_obj* ffi_rray_dimensionality(r_obj* x) {
  return r_int((int) rray_dimensionality(x));
}

r_ssize rray_dimensionality(r_obj* x) {
  check_array(x);

  r_obj* dimension_sizes = r_dim(x);

  if (dimension_sizes == r_null) {
    return 1;
  } else {
    return r_length(dimension_sizes);
  }
}
