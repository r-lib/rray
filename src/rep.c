#include "rep.h"

#include <limits.h>

#include "axes.h"
#include "dimensionality.h"
#include "slice.h"
#include "utils.h"

#include "decl/rep-decl.h"

r_obj* ffi_rray_rep(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axes,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_rep(ffi_x, ffi_times, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
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

  times = KEEP(arg_as_rep_times(times, axes_size, rray_args.times, error_call));
  const int* v_times = r_int_cbegin(times);
  const r_ssize times_size = r_length(times);

  r_obj* indices = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(indices, i, r_true);
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int times = v_times[times_size == 1 ? 0 : i];
    const int dimension = v_x_dimensions[axis - 1];
    const int out_dimension = rray_rep_dimension(dimension, times, error_call);
    r_list_poke(
      indices,
      axis - 1,
      rray_rep_locations(dimension, out_dimension, times)
    );
  }

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(4);
  return out;
}

static r_obj* arg_as_rep_times(
  r_obj* times,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  times = KEEP(arg_as_non_negative_bare_integer(times, arg, error_call));

  const r_ssize times_size = r_length(times);

  if (times_size != 1 && times_size != axes_size) {
    stop_rep_times_size(times_size, axes_size, arg, error_call);
  }

  FREE(1);
  return times;
}

static r_no_return void stop_rep_times_size(
  r_ssize times_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axes_size == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      times_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or size %" R_PRI_SSIZE " to match `axes`, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axes_size,
      times_size
    );
  }
}

static int rray_rep_dimension(
  int dimension,
  int times,
  struct r_lazy error_call
) {
  if (times != 0 && dimension > INT_MAX / times) {
    stop_rep_dimension_too_large(error_call);
  }

  return dimension * times;
}

r_no_return void stop_rep_dimension_too_large(struct r_lazy error_call) {
  r_abort_lazy_call(
    error_call,
    "The dimension implied by `times` is too large for R."
  );
}

static r_obj* rray_rep_locations(int dimension, int out_dimension, int times) {
  r_obj* out = KEEP(r_alloc_integer(out_dimension));
  int* v_out = r_int_begin(out);

  int out_i = 0;

  for (int time = 0; time < times; ++time) {
    for (int i = 0; i < dimension; ++i) {
      v_out[out_i] = i + 1;
      ++out_i;
    }
  }

  FREE(1);
  return out;
}
