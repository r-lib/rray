#include "dimensionality.h"
#include "rlang.h"

r_obj* ffi_rray_dimensionality(r_obj* x) {
  return r_int((int) rray_dimensionality(x));
}

r_ssize rray_dimensionality(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
  case R_TYPE_list:
    break;
  default:
    r_abort_lazy_call(
      r_lazy_null,
      "`x` must be an array, not %s.",
      r_obj_type_friendly(x)
    );
  }

  r_obj* dimensions = r_dim(x);

  if (dimensions == r_null) {
    return 1;
  } else {
    return r_length(dimensions);
  }
}
