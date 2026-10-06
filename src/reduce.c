#include "reduce.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "reduce-names.h"
#include "size.h"
#include "strides.h"
#include "type.h"
#include "utils.h"

r_obj* rray_reduce(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  for (r_ssize i = 0; i < axes_size; ++i) {
    v_out_dimensions[v_axes[i] - 1] = 1;
  }

  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  r_ssize v_out_broadcast_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_out_dimensions,
    dimensionality,
    dimensionality,
    v_out_broadcast_strides
  );

  const rray_reduce_fn fn = fn_switch(x, na_rm, arg, error_call);

  r_obj* out = KEEP(fn(
    x,
    out_size,
    v_x_dimensions,
    dimensionality,
    v_out_broadcast_strides,
    error_call
  ));
  r_attrib_poke_dim(out, out_dimensions);

  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}

r_obj* rray_reduce2(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce2_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_dimensionality(dimensionality);

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(r_alloc_integer(dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);
  r_memcpy(v_out_dimensions, v_x_dimensions, sizeof(int) * dimensionality);
  for (r_ssize i = 0; i < axes_size; ++i) {
    v_out_dimensions[v_axes[i] - 1] = 1;
  }

  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  r_ssize v_out_broadcast_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_out_dimensions,
    dimensionality,
    dimensionality,
    v_out_broadcast_strides
  );

  const rray_reduce2_fn fn = fn_switch(x, arg, error_call);

  r_obj* out = KEEP(fn(
    x,
    na_rm,
    out_size,
    v_x_dimensions,
    dimensionality,
    v_out_broadcast_strides,
    error_call
  ));
  r_attrib_poke_dim(out, out_dimensions);

  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}

r_no_return void stop_unsupported_reduce(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't compute the %s of %s.",
    op,
    rray_arg_type_format(arg, rray_typeof(x))
  );
}
