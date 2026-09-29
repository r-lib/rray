#include "rep-each.h"

#include <limits.h>

#include "axes.h"
#include "dimensionality.h"
#include "rep.h"
#include "size.h"
#include "slice.h"
#include "utils.h"

#include "decl/rep-each-decl.h"

r_obj* ffi_rray_rep_each(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_rep_each(ffi_x, ffi_times, axis, rray_args.x, error_call);
}

r_obj* rray_rep_each(
  r_obj* x,
  r_obj* times,
  int axis,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  r_obj* x_dimensions = r_dim(x);
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);
  const int axis_dimension = v_x_dimensions[axis - 1];

  times = KEEP(
    arg_as_rep_each_times(times, axis_dimension, rray_args.times, error_call)
  );
  const int* v_times = r_int_cbegin(times);
  const r_ssize times_size = r_length(times);

  const int out_dimension =
    rray_rep_each_dimension(axis_dimension, v_times, times_size, error_call);

  int v_out_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  v_out_dimensions[axis - 1] = out_dimension;

  rray_size_from_dimensions_checked(
    v_out_dimensions,
    dimensionality,
    error_call
  );

  r_obj* indices = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(indices, i, r_true);
  }

  r_list_poke(
    indices,
    axis - 1,
    rray_rep_each_locations(axis_dimension, out_dimension, v_times, times_size)
  );

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(3);
  return out;
}

static r_obj* arg_as_rep_each_times(
  r_obj* times,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  times = KEEP(arg_as_non_negative_bare_integer(times, arg, error_call));

  const r_ssize times_size = r_length(times);

  if (times_size != 1 && times_size != axis_dimension) {
    stop_rep_each_times_size(times_size, axis_dimension, arg, error_call);
  }

  FREE(1);
  return times;
}

static r_no_return void stop_rep_each_times_size(
  r_ssize times_size,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  if (axis_dimension == 1) {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1, not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      times_size
    );
  } else {
    r_abort_lazy_call(
      error_call,
      "%s must be size 1 or the `axis` dimension of %d, "
      "not size %" R_PRI_SSIZE ".",
      rray_arg_format(arg),
      axis_dimension,
      times_size
    );
  }
}

static int rray_rep_each_dimension(
  int axis_dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
) {
  int out = 0;

  for (int i = 0; i < axis_dimension; ++i) {
    const int times = v_times[times_size == 1 ? 0 : i];

    if (out > INT_MAX - times) {
      stop_rep_dimension_too_large(error_call);
    }

    out += times;
  }

  return out;
}

static r_obj* rray_rep_each_locations(
  int axis_dimension,
  int out_dimension,
  const int* v_times,
  r_ssize times_size
) {
  r_obj* out = KEEP(r_alloc_integer(out_dimension));
  int* v_out = r_int_begin(out);

  int out_i = 0;

  for (int i = 0; i < axis_dimension; ++i) {
    const int times = v_times[times_size == 1 ? 0 : i];

    for (int time = 0; time < times; ++time) {
      v_out[out_i] = i + 1;
      ++out_i;
    }
  }

  FREE(1);
  return out;
}
