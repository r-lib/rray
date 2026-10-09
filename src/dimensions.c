#include "dimensions.h"

#include "dimensionality.h"
#include "size.h"
#include "utils.h"
#include "wrapper.h"

#include "decl/dimensions-decl.h"

r_obj* ffi_rray_dimensions(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_dimensions(ffi_x, rray_args.x, error_call);
}

r_obj* rray_dimensions(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));
  r_obj* out = r_dim(x);
  FREE(1);
  return out;
}

int rray_dimension(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_obj* dimensions = rray_dimensions(x, arg, error_call);
  return r_int_get(dimensions, axis - 1);
}

bool rray_dimensions_are_equal(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_y_dimensions,
  int y_dimensionality
) {
  if (x_dimensionality != y_dimensionality) {
    return false;
  }

  for (int i = 0; i < x_dimensionality; ++i) {
    if (v_x_dimensions[i] != v_y_dimensions[i]) {
      return false;
    }
  }

  return true;
}

r_obj* ffi_rray_set_dimensions(
  r_obj* ffi_x,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_dimensions(ffi_x, ffi_dimensions, rray_args.x, error_call);
}

r_obj* rray_set_dimensions(
  r_obj* x,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));
  dimensions =
    KEEP(arg_as_dimensions(dimensions, rray_args.dimensions, error_call));

  const r_ssize x_size = rray_size(x, arg, error_call);

  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  const int* v_dimensions = r_int_cbegin(dimensions);

  const r_ssize size = rray_size_from_dimensions(v_dimensions, dimensionality);

  if (x_size != size) {
    r_abort_lazy_call(
      error_call,
      "Can't set these dimensions. "
      "Can't change from a size of %" R_PRI_SSIZE " to a size of %" R_PRI_SSIZE
      ".",
      x_size,
      size
    );
  }

  r_obj* out = KEEP(r_wrap(x));

  r_attrib_poke_dim_names(out, r_null);
  r_attrib_poke_dim(out, dimensions);

  FREE(3);
  return out;
}

r_obj* ffi_rray_dimensions_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_dimensions_common(
    ffi_xs,
    ffi_dimensions,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (dimensions != r_null) {
    return arg_as_dimensions(dimensions, rray_args.dot_dimensions, error_call);
  }

  return rray_dimensions_common_opts(xs, NULL, 0, arg, error_call);
}

r_obj* rray_dimensions_common_opts(
  r_obj* xs,
  const int* v_ignore,
  r_ssize ignore_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize n = r_length(xs);

  if (n == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* x_arg = new_subscript_arg(arg, xs_names, n, &x_i);
  KEEP(x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* out_arg = new_subscript_arg(arg, xs_names, n, &out_i);
  KEEP(out_arg->shelter);

  int out_dimensionality = 1;

  // Stack allocated array of known max size that we accumulate the common
  // dimensions in. Initialized to 1, which works very nicely with broadcasting.
  int v_out_dimensions[RRAY_MAX_DIMENSIONALITY];

  // Index of the input that set each axis of `v_out_dimensions`, always
  // populated by the time that axis can conflict
  r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];

  for (int i = 0; i < RRAY_MAX_DIMENSIONALITY; ++i) {
    v_out_dimensions[i] = 1;
    v_out_args[i] = 0;
  }

  bool v_ignored[RRAY_MAX_DIMENSIONALITY];
  r_memset(v_ignored, 0, sizeof(bool) * RRAY_MAX_DIMENSIONALITY);

  for (r_ssize i = 0; i < ignore_size; ++i) {
    v_ignored[v_ignore[i] - 1] = true;
  }

  for (; x_i < n; ++x_i) {
    r_obj* x = v_xs[x_i];

    r_obj* x_dimensions = KEEP(rray_dimensions(x, x_arg, error_call));
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality =
      rray_dimensionality_from_dimensions(x_dimensions);
    check_dimensionality(x_dimensionality);

    // Update `v_out_dimensions` and `out_dimensionality` in place
    // with common dimensions
    rray_dimensions_merge(
      v_out_dimensions,
      v_out_args,
      v_ignored,
      &out_dimensionality,
      &out_i,
      v_x_dimensions,
      x_dimensionality,
      x_i,
      out_arg,
      x_arg,
      error_call
    );

    FREE(1);
  }

  r_obj* out = KEEP(r_alloc_integer(out_dimensionality));
  int* v_out = r_int_begin(out);
  r_memcpy(v_out, v_out_dimensions, sizeof(int) * out_dimensionality);

  FREE(4);
  return out;
}

static inline void rray_dimensions_merge(
  int* v_out_dimensions,
  r_ssize* v_out_args,
  const bool* v_ignored,
  int* p_out_dimensionality,
  r_ssize* p_out_i,
  const int* v_x_dimensions,
  int x_dimensionality,
  r_ssize x_i,
  struct rray_arg* out_arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  const int out_dimensionality = *p_out_dimensionality;

  const int common_dimensionality = (out_dimensionality > x_dimensionality)
    ? out_dimensionality
    : x_dimensionality;

  *p_out_dimensionality = common_dimensionality;

  for (int i = 0; i < common_dimensionality; ++i) {
    if (v_ignored[i]) {
      continue;
    }

    const int out_dimension =
      (i < out_dimensionality) ? v_out_dimensions[i] : 1;
    const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;

    *p_out_i = v_out_args[i];

    const int dimension = rray_dimension2(
      out_dimension,
      x_dimension,
      i + 1,
      out_arg,
      x_arg,
      error_call
    );

    if (dimension != out_dimension) {
      v_out_dimensions[i] = dimension;
      v_out_args[i] = x_i;
    }
  }
}

r_obj* rray_dimensions2(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_y_dimensions,
  int y_dimensionality,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const int dimensionality =
    (x_dimensionality > y_dimensionality) ? x_dimensionality : y_dimensionality;

  check_dimensionality(dimensionality);

  r_obj* out = KEEP(r_alloc_integer(dimensionality));
  int* v_out = r_int_begin(out);

  for (int i = 0; i < dimensionality; ++i) {
    const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;
    const int y_dimension = (i < y_dimensionality) ? v_y_dimensions[i] : 1;

    v_out[i] = rray_dimension2(
      x_dimension,
      y_dimension,
      i + 1,
      x_arg,
      y_arg,
      error_call
    );
  }

  FREE(1);
  return out;
}

static inline int rray_dimension2(
  int x,
  int y,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  if (x == y) {
    return x;
  }

  if (x == 1) {
    return y;
  }

  if (y == 1) {
    return x;
  }

  stop_incompatible_dimensions(x, y, axis, x_arg, y_arg, error_call);
}

static r_no_return void stop_incompatible_dimensions(
  int x,
  int y,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't find common dimensions at axis %d. "
    "%s has dimension %d and %s has dimension %d.",
    axis,
    rray_arg_format(x_arg),
    x,
    rray_arg_format(y_arg),
    y
  );
}

r_obj* arg_as_dimensions(
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  dimensions =
    KEEP(arg_as_non_negative_bare_integer(dimensions, arg, error_call));

  if (rray_dimensionality_from_dimensions(dimensions) == 0) {
    r_abort_lazy_call(
      error_call,
      "%s must have at least one element.",
      rray_arg_format(arg)
    );
  }

  FREE(1);
  return dimensions;
}
