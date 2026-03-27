#include "dimensions.h"

#include "utils.h"

r_obj* ffi_rray_dimensions(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimensions(x, error_call);
}

r_obj* rray_dimensions(r_obj* x, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim(x);
  FREE(1);
  return out;
}

bool rray_dimensions_are_equal(
  const int* v_x_dimensions,
  r_ssize x_dimensionality,
  const int* v_y_dimensions,
  r_ssize y_dimensionality
) {
  if (x_dimensionality != y_dimensionality) {
    return false;
  }

  for (r_ssize i = 0; i < x_dimensionality; ++i) {
    if (v_x_dimensions[i] != v_y_dimensions[i]) {
      return false;
    }
  }

  return true;
}

r_obj* arg_as_dimensions(r_obj* dimensions, struct r_lazy error_call) {
  if (r_typeof(dimensions) != R_TYPE_integer) {
    dimensions =
      vec_cast(dimensions, r_globals.empty_int, dimensions_chr, r_null);
  }
  KEEP(dimensions);

  if (r_attrib_has_any(dimensions)) {
    r_abort_lazy_call(error_call, "`dimensions` can't have attributes.");
  }

  const r_ssize dimensionality = r_length(dimensions);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "`dimensions` must have at least one element."
    );
  }

  const int* v_dimensions = r_int_cbegin(dimensions);

  for (r_ssize i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];

    if (dimension == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`dimensions` must not contain missing values."
      );
    }

    if (dimension < 0) {
      r_abort_lazy_call(
        error_call,
        "`dimensions` must not contain negative values."
      );
    }
  }

  FREE(1);
  return dimensions;
}
