#include "dimensionality.h"

r_obj* ffi_rray_dimensionality(r_obj* x) {
  return r_int((int) rray_dimensionality(x));
}

r_ssize rray_dimensionality(r_obj* x) {
  r_obj* dimensions = r_dim(x);

  if (dimensions == r_null) {
    return 1;
  } else {
    return r_length(dimensions);
  }
}
