#include "extract-subscript.h"

#include <limits.h>
#include <math.h>

#include "dimensions.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

#include "decl/extract-subscript-decl.h"

r_obj* ffi_rray_as_extract_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  r_obj* dimensions =
    KEEP(arg_as_dimensions(ffi_dimensions, rray_args.dimensions, error_call));

  r_obj* out = rray_as_extract_subscript(
    ffi_i,
    r_int_cbegin(dimensions),
    (int) r_length(dimensions),
    rray_args.i,
    error_call
  );

  FREE(1);
  return out;
}

r_obj* rray_as_extract_subscript(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(i, i_arg, error_call);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  switch (r_typeof(i)) {
  case R_TYPE_logical:
    return rray_as_extract_mask(
      i,
      v_dimensions,
      dimensionality,
      size,
      i_arg,
      error_call
    );
  case R_TYPE_integer:
  case R_TYPE_double:
    break;
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be logical, integer, or double, not %s.",
      rray_arg_format(i_arg),
      r_obj_type_friendly(i)
    );
  }

  r_obj* i_dimensions = r_dim(i);
  const r_ssize i_dimensionality =
    i_dimensions == r_null ? 1 : r_length(i_dimensions);

  switch (i_dimensionality) {
  case 1:
    return rray_as_extract_positions(i, size, i_arg, error_call);
  case 2:
    return rray_as_extract_points(
      i,
      v_dimensions,
      dimensionality,
      i_arg,
      error_call
    );
  default:
    r_abort_lazy_call(
      error_call,
      "Numeric %s must be a vector or a matrix, not an array with "
      "%" R_PRI_SSIZE " dimensions.",
      rray_arg_format(i_arg),
      i_dimensionality
    );
  }
}

static r_obj* rray_as_extract_mask(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_obj* i_dimensions = r_dim(i);

  if (i_dimensions != r_null && r_length(i_dimensions) != 1) {
    const bool equal = rray_dimensions_are_equal(
      r_int_cbegin(i_dimensions),
      (int) r_length(i_dimensions),
      v_dimensions,
      dimensionality
    );

    if (!equal) {
      r_abort_lazy_call(
        error_call,
        "Logical %s must be a vector or have the same dimensions as `x`.",
        rray_arg_format(i_arg)
      );
    }
  }

  const r_ssize i_size = r_length(i);

  if (i_size != 1 && i_size != size) {
    r_abort_lazy_call(
      error_call,
      "Logical %s must be size 1 or %" R_PRI_SSIZE ", not %" R_PRI_SSIZE ".",
      rray_arg_format(i_arg),
      size,
      i_size
    );
  }

  return vec_bare(i);
}

static r_obj* rray_as_extract_positions(
  r_obj* i,
  r_ssize size,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  i = KEEP(rray_as_extract_integer(i, i_arg, error_call));

  const r_ssize i_size = r_length(i);
  const int* v_i = r_int_cbegin(i);

  bool any_positive = false;
  bool any_negative = false;
  bool any_zero = false;
  bool any_missing = false;

  for (r_ssize j = 0; j < i_size; ++j) {
    const int elt = v_i[j];

    if (elt == r_globals.na_int) {
      any_missing = true;
      continue;
    }
    if (elt > size) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain values greater than %" R_PRI_SSIZE ".",
        rray_arg_format(i_arg),
        size
      );
    }
    if (elt < -size) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain values less than -%" R_PRI_SSIZE ".",
        rray_arg_format(i_arg),
        size
      );
    }

    any_positive |= elt > 0;
    any_negative |= elt < 0;
    any_zero |= elt == 0;
  }

  if (any_positive && any_negative) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix positive and negative values.",
      rray_arg_format(i_arg)
    );
  }
  if (any_negative && any_missing) {
    r_abort_lazy_call(
      error_call,
      "%s can't mix negative and missing values.",
      rray_arg_format(i_arg)
    );
  }

  r_obj* out = i;

  if (any_negative) {
    out = rray_as_extract_complement(v_i, i_size, size);
  } else if (any_zero) {
    out = rray_as_extract_nonzero(v_i, i_size);
  }

  FREE(1);
  return out;
}

static r_obj* rray_as_extract_points(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_obj* i_dimensions = r_dim(i);
  const int* v_i_dimensions = r_int_cbegin(i_dimensions);
  const r_ssize size = v_i_dimensions[0];
  const int columns = v_i_dimensions[1];

  if (columns != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Numeric matrix %s must have %d column%s, one for each axis of `x`, "
      "not %d.",
      rray_arg_format(i_arg),
      dimensionality,
      dimensionality == 1 ? "" : "s",
      columns
    );
  }

  r_obj* out = KEEP(rray_as_extract_integer(i, i_arg, error_call));
  r_attrib_poke_dim(out, i_dimensions);

  const int* v_out = r_int_cbegin(out);

  for (int axis = 0; axis < columns; ++axis) {
    const int dimension = v_dimensions[axis];
    const int* v_column = v_out + axis * size;

    for (r_ssize j = 0; j < size; ++j) {
      const int elt = v_column[j];

      if (elt == r_globals.na_int) {
        continue;
      }
      if (elt < 1) {
        r_abort_lazy_call(
          error_call,
          "Column %d of %s must only contain positive values or missing "
          "values.",
          axis + 1,
          rray_arg_format(i_arg)
        );
      }
      if (elt > dimension) {
        r_abort_lazy_call(
          error_call,
          "Column %d of %s must not contain values greater than %d.",
          axis + 1,
          rray_arg_format(i_arg),
          dimension
        );
      }
    }
  }

  FREE(1);
  return out;
}

static r_obj* rray_as_extract_integer(
  r_obj* i,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  if (r_typeof(i) == R_TYPE_integer) {
    return vec_bare(i);
  }

  const r_ssize size = r_length(i);
  const double* v_i = r_dbl_cbegin(i);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize j = 0; j < size; ++j) {
    const double elt = v_i[j];

    if (isnan(elt)) {
      v_out[j] = r_globals.na_int;
      continue;
    }
    if (elt != trunc(elt) || elt > INT_MAX || elt < -INT_MAX) {
      r_abort_lazy_call(
        error_call,
        "Can't convert from %s <double> to <integer> due to loss of "
        "precision.",
        rray_arg_format(i_arg)
      );
    }

    v_out[j] = (int) elt;
  }

  FREE(1);
  return out;
}

static r_obj* rray_as_extract_complement(
  const int* v_i,
  r_ssize i_size,
  r_ssize size
) {
  r_obj* out = r_alloc_logical(size);
  int* v_out = r_lgl_begin(out);

  for (r_ssize j = 0; j < size; ++j) {
    v_out[j] = 1;
  }

  for (r_ssize j = 0; j < i_size; ++j) {
    const int elt = v_i[j];

    if (elt != 0) {
      v_out[-elt - 1] = 0;
    }
  }

  return out;
}

static r_obj* rray_as_extract_nonzero(const int* v_i, r_ssize i_size) {
  r_ssize size = 0;

  for (r_ssize j = 0; j < i_size; ++j) {
    size += v_i[j] != 0;
  }

  r_obj* out = r_alloc_integer(size);
  int* v_out = r_int_begin(out);

  r_ssize k = 0;

  for (r_ssize j = 0; j < i_size; ++j) {
    const int elt = v_i[j];

    if (elt != 0) {
      v_out[k] = elt;
      ++k;
    }
  }

  return out;
}

static r_obj* vec_bare(r_obj* x) {
  if (!r_attrib_has_any(x)) {
    return x;
  }

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_zap_all(out);

  FREE(1);
  return out;
}
