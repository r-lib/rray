#include "extract-subscript.h"

#include "dimensionality.h"
#include "dimensions.h"
#include "size.h"
#include "utils.h"

#include "decl/extract-subscript-decl.h"

r_obj* ffi_rray_as_extract_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};

  r_obj* dimensions =
    KEEP(arg_as_dimensions(ffi_dimensions, rray_args.dimensions, error_call));

  const struct rray_subscript subscript = rray_as_extract_subscript(
    ffi_i,
    r_int_cbegin(dimensions),
    rray_dimensionality_from_dimensions(dimensions),
    rray_args.i,
    error_call
  );
  KEEP(subscript.index);

  r_obj* out = rray_subscript_as_list(subscript);

  FREE(2);
  return out;
}

struct rray_subscript rray_as_extract_subscript(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  check_unclassed(index, index_arg, error_call);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  switch (r_typeof(index)) {
  case R_TYPE_logical:
    return rray_as_extract_mask(
      index,
      v_dimensions,
      dimensionality,
      size,
      index_arg,
      error_call
    );
  case R_TYPE_integer:
  case R_TYPE_double: {
    r_obj* index_dimensions = r_dim(index);
    const int index_dimensionality = index_dimensions == r_null
      ? 1
      : rray_dimensionality_from_dimensions(index_dimensions);

    switch (index_dimensionality) {
    case 1:
      return rray_as_subscript_locations(index, size, index_arg, error_call);
    case 2:
      return rray_as_extract_points(
        index,
        v_dimensions,
        dimensionality,
        index_arg,
        error_call
      );
    default:
      r_abort_lazy_call(
        error_call,
        "Numeric %s must be a vector or a matrix, not an array with a "
        "dimensionality of %d.",
        rray_arg_format(index_arg),
        index_dimensionality
      );
    }
  }
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be logical, integer, or double, not %s.",
      rray_arg_format(index_arg),
      r_obj_type_friendly(index)
    );
  }
}

static struct rray_subscript rray_as_extract_mask(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  r_obj* index_dimensions = r_dim(index);
  const int index_dimensionality = index_dimensions == r_null
    ? 1
    : rray_dimensionality_from_dimensions(index_dimensions);

  if (index_dimensionality == 1) {
    return rray_as_subscript_mask(index, size, index_arg, error_call);
  }

  // If a logical array is provided, it must match `x` dimensions exactly. In
  // theory it could broadcast but likely not worth it.
  const bool equal = rray_dimensions_are_equal(
    r_int_cbegin(index_dimensions),
    index_dimensionality,
    v_dimensions,
    dimensionality
  );

  if (!equal) {
    r_abort_lazy_call(
      error_call,
      "Logical array %s must have the same dimensions as `x`.",
      rray_arg_format(index_arg)
    );
  }

  return (struct rray_subscript){
    .index = index,
    .kind = RRAY_SUBSCRIPT_KIND_mask,
    .size = rray_mask_size(index, size)
  };
}

static struct rray_subscript rray_as_extract_points(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* index_arg,
  struct r_lazy error_call
) {
  const int* v_index_dimensions = r_int_cbegin(r_dim(index));
  const r_ssize rows = v_index_dimensions[0];
  const int columns = v_index_dimensions[1];

  if (columns != dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Numeric matrix %s must have %d column%s, one for each axis of `x`, "
      "not %d.",
      rray_arg_format(index_arg),
      dimensionality,
      dimensionality == 1 ? "" : "s",
      columns
    );
  }

  for (int column = 0; column < columns; ++column) {
    const int dimension = v_dimensions[column];
    const struct rray_subscript_summary summary =
      rray_subscript_summarise(index, column * rows, rows);

    if (summary.any_fractional) {
      stop_subscript_fractional(index_arg, error_call);
    }
    if (summary.min < 1) {
      r_abort_lazy_call(
        error_call,
        "Column %d of %s must only contain positive values or missing "
        "values.",
        column + 1,
        rray_arg_format(index_arg)
      );
    }
    if (summary.max > dimension) {
      r_abort_lazy_call(
        error_call,
        "Column %d of %s must not contain values greater than %d.",
        column + 1,
        rray_arg_format(index_arg),
        dimension
      );
    }
  }

  return (struct rray_subscript){
    .index = index,
    .kind = r_typeof(index) == R_TYPE_integer ? RRAY_SUBSCRIPT_KIND_points_int
                                              : RRAY_SUBSCRIPT_KIND_points_dbl,
    .size = rows
  };
}
