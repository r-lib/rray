#include "roll.h"

#include "axes.h"
#include "dimensionality.h"
#include "slice.h"
#include "utils.h"

#include "decl/roll-decl.h"

r_obj* ffi_rray_roll(
  r_obj* ffi_x,
  r_obj* ffi_n,
  r_obj* ffi_axes,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_roll(ffi_x, ffi_n, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_roll(
  r_obj* x,
  r_obj* n,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  n = KEEP(arg_as_roll_n(n, axes_size, rray_args.n, error_call));
  const int* v_n = r_int_cbegin(n);
  const r_ssize n_size = r_length(n);

  r_obj* indices = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(indices, i, r_true);
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int dimension = v_x_dimensions[axis - 1];
    const int shift = rray_roll_shift(v_n[n_size == 1 ? 0 : i], dimension);
    r_list_poke(indices, axis - 1, rray_roll_locations(dimension, shift));
  }

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(4);
  return out;
}

static r_obj* arg_as_roll_n(
  r_obj* n,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  n = KEEP(arg_as_bare_integer(n, arg, error_call));
  check_roll_n_not_missing(n, arg, error_call);

  const r_ssize n_size = r_length(n);

  if (n_size != 1 && n_size != axes_size) {
    stop_roll_n_size(n_size, axes_size, arg, error_call);
  }

  FREE(1);
  return n;
}

static r_no_return void stop_roll_n_size(
  r_ssize n_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axes_size == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      n_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or size %" R_PRI_SSIZE " to match `axes`, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axes_size,
      n_size
    );
  }
}

static r_obj* rray_roll_locations(int dimension, int n) {
  r_obj* out = KEEP(r_alloc_integer(dimension));
  int* v_out = r_int_begin(out);

  for (int i = 0; i < dimension; ++i) {
    v_out[i] = (i >= n) ? i - n + 1 : i - n + dimension + 1;
  }

  FREE(1);
  return out;
}

static void check_roll_n_not_missing(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize n_size = r_length(n);
  const int* v_n = r_int_cbegin(n);

  for (r_ssize i = 0; i < n_size; ++i) {
    if (v_n[i] == r_globals.na_int) {
      r_abort_lazy_call(
        error_call,
        "%s must not contain missing values.",
        rray_arg_format(arg)
      );
    }
  }
}

static inline int rray_roll_shift(int n, int dimension) {
  if (dimension == 0) {
    return 0;
  }

  int out = n % dimension;

  if (out < 0) {
    out += dimension;
  }

  return out;
}
