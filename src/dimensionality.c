#include "dimensionality.h"

#include "dimensions.h"

r_obj* ffi_rray_dimensionality(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return r_int((int) rray_dimensionality(ffi_x, error_call));
}

r_ssize rray_dimensionality(r_obj* x, struct r_lazy error_call) {
  r_obj* dimensions = rray_dimensions(x, error_call);
  return rray_dimensionality_from_dimensions(dimensions);
}

r_ssize rray_dimensionality_from_dimensions(r_obj* dimensions) {
  return r_length(dimensions);
}

void check_max_dimensionality(r_ssize dimensionality) {
  if (dimensionality > RRAY_MAX_DIMENSIONALITY) {
    r_abort(
      "rray can't support arrays with a dimensionality greater than %i. "
      "A dimensionality of %i was requested.",
      (int) RRAY_MAX_DIMENSIONALITY,
      (int) dimensionality
    );
  }
}
