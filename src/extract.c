#include "extract.h"

#include "dimensions.h"
#include "index.h"
#include "utils.h"
#include "wrapper.h"

#include "decl/extract-decl.h"

r_obj* ffi_rray_extract(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_extract(ffi_x, ffi_i, rray_args.x, rray_args.i, error_call);
}

r_obj* rray_extract(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  check_unclassed(i, i_arg, error_call);

  r_obj* i_dimensions = r_dim(i);
  const r_ssize i_dimensionality =
    i_dimensions == r_null ? 1 : r_length(i_dimensions);

  r_obj* out;

  switch (r_typeof(i)) {
  case R_TYPE_null:
    out = rray_extract_flat(x, i, x_arg, i_arg, error_call);
    break;
  case R_TYPE_logical:
    out = rray_extract_mask(x, i, x_arg, i_arg, error_call);
    break;
  case R_TYPE_integer:
  case R_TYPE_double:
    if (i_dimensionality == 1) {
      out = rray_extract_flat(x, i, x_arg, i_arg, error_call);
    } else if (i_dimensionality == 2) {
      out = rray_extract_points(x, i, x_arg, i_arg, error_call);
    } else {
      r_abort_lazy_call(
        error_call,
        "Numeric %s must be a vector or a matrix, not an array with "
        "%" R_PRI_SSIZE " dimensions.",
        rray_arg_format(i_arg),
        i_dimensionality
      );
    }
    break;
  default:
    r_abort_lazy_call(
      error_call,
      "%s must be logical, integer, or double, not %s.",
      rray_arg_format(i_arg),
      r_obj_type_friendly(i)
    );
  }

  FREE(1);
  return out;
}

static r_obj* rray_extract_flat(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  const r_ssize size = r_length(x);

  r_obj* location = KEEP(vec_as_location(i, size, i_arg, error_call));

  r_obj* indices = KEEP(r_alloc_list(1));
  r_list_poke(indices, 0, location);

  r_obj* dimensions = KEEP(r_int(r_ssize_as_integer(size)));
  r_obj* flat = KEEP(rray_set_dimensions(x, dimensions, x_arg, error_call));

  r_obj* out = rray_index(flat, indices, x_arg, i_arg, error_call);

  FREE(4);
  return out;
}

static r_obj* rray_extract_mask(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_obj* i_dimensions = r_dim(i);

  if (i_dimensions == r_null) {
    return rray_extract_flat(x, i, x_arg, i_arg, error_call);
  }

  r_obj* x_dimensions = r_dim(x);
  const int i_dimensionality = (int) r_length(i_dimensions);
  const int x_dimensionality = (int) r_length(x_dimensions);

  const bool equal = rray_dimensions_are_equal(
    r_int_cbegin(i_dimensions),
    i_dimensionality,
    r_int_cbegin(x_dimensions),
    x_dimensionality
  );

  if (i_dimensionality != 1 && !equal) {
    r_abort_lazy_call(
      error_call,
      "Logical %s must be a vector or have the same dimensions as `x`.",
      rray_arg_format(i_arg)
    );
  }

  i = KEEP(r_wrap(i));
  r_attrib_poke_dim(i, r_null);

  r_obj* out = rray_extract_flat(x, i, x_arg, i_arg, error_call);

  FREE(1);
  return out;
}

static r_obj* rray_extract_points(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const r_ssize x_dimensionality = r_length(x_dimensions);

  const int* v_i_dimensions = r_int_cbegin(r_dim(i));
  const r_ssize size = v_i_dimensions[0];
  const r_ssize columns = v_i_dimensions[1];

  if (columns != x_dimensionality) {
    r_abort_lazy_call(
      error_call,
      "Numeric matrix %s must have %" R_PRI_SSIZE " column%s, one for each "
      "axis of `x`, not %" R_PRI_SSIZE ".",
      rray_arg_format(i_arg),
      x_dimensionality,
      x_dimensionality == 1 ? "" : "s",
      columns
    );
  }

  r_obj* indices = KEEP(r_alloc_list(columns));

  r_ssize j = 0;
  struct rray_arg column_arg = new_column_arg(i_arg, &j);

  for (; j < columns; ++j) {
    r_obj* index = KEEP(rray_extract_column(i, size, j));
    index = KEEP(arg_as_bare_integer(index, &column_arg, error_call));
    index =
      rray_as_index_array(index, v_x_dimensions[j], &column_arg, error_call);
    r_list_poke(indices, j, index);
    FREE(2);
  }

  r_obj* out = rray_index(x, indices, x_arg, i_arg, error_call);

  FREE(1);
  return out;
}

static r_obj* rray_extract_column(r_obj* x, r_ssize size, r_ssize j) {
  switch (r_typeof(x)) {
  case R_TYPE_integer: {
    r_obj* out = r_alloc_integer(size);
    r_memcpy(
      r_int_begin(out),
      r_int_cbegin(x) + j * size,
      (size_t) size * sizeof(int)
    );
    return out;
  }
  case R_TYPE_double: {
    r_obj* out = r_alloc_double(size);
    r_memcpy(
      r_dbl_begin(out),
      r_dbl_cbegin(x) + j * size,
      (size_t) size * sizeof(double)
    );
    return out;
  }
  default:
    r_stop_unreachable();
  }
}
