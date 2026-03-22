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

void check_dimension_sizes(r_obj* dimension_sizes, struct r_lazy error_call) {
  if (r_typeof(dimension_sizes) != R_TYPE_integer) {
    r_abort_lazy_call(
      error_call,
      "`dimension_sizes` must be an integer vector, not %s.",
      r_obj_type_friendly(dimension_sizes)
    );
  }

  const R_xlen_t dimensionality = r_length(dimension_sizes);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "`dimension_sizes` must have at least one element."
    );
  }

  const int* v_dimension_sizes = r_int_cbegin(dimension_sizes);

  for (R_xlen_t i = 0; i < dimensionality; ++i) {
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
}
