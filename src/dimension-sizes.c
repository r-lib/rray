#include "dimension-sizes.h"

#include "utils.h"

r_obj* ffi_rray_dimension_sizes(r_obj* x, r_obj* frame) {
  struct r_lazy error_call = { .x = frame, .env = r_null };
  return rray_dimension_sizes(x, error_call);
}

r_obj* rray_dimension_sizes(r_obj* x, struct r_lazy error_call) {
  x = KEEP(arg_as_array(x, "x", error_call));
  r_obj* out = r_dim(x);
  FREE(1);
  return out;
}

bool rray_dimension_sizes_are_equal(
  const int* v_x_dimension_sizes,
  r_ssize x_dimensionality,
  const int* v_y_dimension_sizes,
  r_ssize y_dimensionality
) {
  if (x_dimensionality != y_dimensionality) {
    return false;
  }

  for (r_ssize i = 0; i < x_dimensionality; ++i) {
    if (v_x_dimension_sizes[i] != v_y_dimension_sizes[i]) {
      return false;
    }
  }

  return true;
}

r_obj* arg_as_dimension_sizes(
  r_obj* dimension_sizes,
  struct r_lazy error_call
) {
  if (r_typeof(dimension_sizes) != R_TYPE_integer) {
    dimension_sizes = vec_cast(
      dimension_sizes,
      r_globals.empty_int,
      dimension_sizes_chr,
      r_null
    );
  }
  KEEP(dimension_sizes);

  if (r_attrib_has_any(dimension_sizes)) {
    r_abort_lazy_call(error_call, "`dimension_sizes` can't have attributes.");
  }

  const r_ssize dimensionality = r_length(dimension_sizes);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "`dimension_sizes` must have at least one element."
    );
  }

  const int* v_dimension_sizes = r_int_cbegin(dimension_sizes);

  for (r_ssize i = 0; i < dimensionality; ++i) {
    const int dimension_size = v_dimension_sizes[i];

    if (dimension_size == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "`dimension_sizes` must not contain missing values."
      );
    }

    if (dimension_size < 0) {
      r_abort_lazy_call(
        error_call,
        "`dimension_sizes` must not contain negative values."
      );
    }
  }

  FREE(1);
  return dimension_sizes;
}
