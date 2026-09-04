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
      "Can't change from a size of %" R_PRIdXLEN_T
      " to a size of %" R_PRIdXLEN_T ".",
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

r_obj* rray_set_axes_dimension(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size,
  int dimension
) {
  r_obj* out = KEEP(r_alloc_integer(dimensionality));
  int* v_out = r_int_begin(out);

  // Start with `v_dimensions`
  r_memcpy(v_out, v_dimensions, sizeof(int) * dimensionality);

  // Set `axes` to `dimension`
  for (r_ssize i = 0; i < axes_size; ++i) {
    v_out[v_axes[i] - 1] = dimension;
  }

  FREE(1);
  return out;
}

r_obj* ffi_rray_dimensions_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_dimensions_common(ffi_xs, ffi_dimensions, error_call);
}

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
) {
  if (dimensions != r_null) {
    return arg_as_dimensions(dimensions, rray_args.dot_dimensions, error_call);
  }

  const r_ssize n = r_length(xs);

  if (n == 0) {
    r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
  }

  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* x_arg = new_subscript_arg(NULL, xs_names, n, &x_i);
  KEEP(x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* out_arg = new_subscript_arg(NULL, xs_names, n, &out_i);
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

  for (; x_i < n; ++x_i) {
    r_obj* x = v_xs[x_i];

    r_obj* x_dimensions = KEEP(rray_dimensions(x, x_arg, error_call));
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality =
      rray_dimensionality_from_dimensions(x_dimensions);
    check_max_dimensionality(x_dimensionality);

    // Update `v_out_dimensions` and `out_dimensionality` in place
    // with common dimensions
    rray_dimensions2(
      v_out_dimensions,
      v_out_args,
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

static inline void rray_dimensions2(
  int* v_out_dimensions,
  r_ssize* v_out_args,
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
    const int out_dimension =
      (i < out_dimensionality) ? v_out_dimensions[i] : 1;
    const int x_dimension = (i < x_dimensionality) ? v_x_dimensions[i] : 1;

    if (out_dimension == x_dimension) {
      // Nothing to do
      // v_out_dimensions[i] = out_dimension;
    } else if (out_dimension == 1) {
      v_out_dimensions[i] = x_dimension;
      v_out_args[i] = x_i;
    } else if (x_dimension == 1) {
      // Nothing to do
      // v_out_dimensions[i] = out_dimension;
    } else {
      *p_out_i = v_out_args[i];

      r_abort_lazy_call(
        error_call,
        "Can't find common dimensions at axis %d. "
        "%s has dimension %d and %s has dimension %d.",
        i + 1,
        rray_arg_format(out_arg),
        out_dimension,
        rray_arg_format(x_arg),
        x_dimension
      );
    }
  }
}

r_obj* arg_as_dimensions(
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (r_typeof(dimensions) != R_TYPE_integer) {
    dimensions = vec_cast(dimensions, r_globals.empty_int, arg, NULL);
  }
  KEEP(dimensions);

  if (r_attrib_has_any(dimensions)) {
    r_abort_lazy_call(
      error_call,
      "%s can't have attributes.",
      rray_arg_format(arg)
    );
  }

  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  if (dimensionality == 0) {
    r_abort_lazy_call(
      error_call,
      "%s must have at least one element.",
      rray_arg_format(arg)
    );
  }

  const int* v_dimensions = r_int_cbegin(dimensions);

  for (int i = 0; i < dimensionality; ++i) {
    const int dimension = v_dimensions[i];

    if (dimension == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }

    if (dimension < 0) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain negative values.",
        rray_arg_format(arg)
      );
    }
  }

  FREE(1);
  return dimensions;
}
