#include "reduce.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "reduce-names.h"
#include "size.h"
#include "strides.h"
#include "type.h"
#include "utils.h"

#include "decl/reduce-decl.h"

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

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(rray_set_axes_dimension(
    v_x_dimensions,
    dimensionality,
    v_axes,
    axes_size,
    1
  ));
  const int* v_out_dimensions = r_int_cbegin(out_dimensions);

  const r_ssize out_size =
    rray_size_from_dimensions(v_out_dimensions, dimensionality);

  struct rray_strided_iterator_plan plan = rray_broadcast_iterator_plan(
    v_out_dimensions,
    dimensionality,
    v_x_dimensions,
    dimensionality
  );

  const rray_reduce_fn fn = fn_switch(x, na_rm, arg, error_call);

  r_obj* out = KEEP(fn(x, out_size, &plan));
  r_attrib_poke_dim(out, out_dimensions);

  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}

r_obj* rray_reduce_nested(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_nested_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);

  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);
  check_max_dimensionality(dimensionality);

  r_ssize v_x_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_strides_from_dimensions(
    v_x_dimensions,
    dimensionality,
    v_x_strides
  );

  axes = KEEP(arg_as_axes(axes, dimensionality, rray_args.axes, error_call));
  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out_dimensions = KEEP(rray_set_axes_dimension(
    v_x_dimensions,
    dimensionality,
    v_axes,
    axes_size,
    1
  ));

  r_obj* retained_axes =
    KEEP(rray_axes_complement(v_axes, axes_size, dimensionality));
  const int* v_retained_axes = r_int_cbegin(retained_axes);
  const r_ssize retained_axes_size = r_length(retained_axes);

  const struct rray_strided_iterator_plan outer_plan = rray_reduce_axes_plan(
    v_x_dimensions,
    v_x_strides,
    v_retained_axes,
    retained_axes_size
  );

  const struct rray_strided_iterator_plan inner_plan =
    rray_reduce_axes_plan(v_x_dimensions, v_x_strides, v_axes, axes_size);

  const rray_reduce_nested_fn fn = fn_switch(x, na_rm, arg, error_call);

  r_obj* out = KEEP(fn(x, &outer_plan, &inner_plan));
  r_attrib_poke_dim(out, out_dimensions);

  r_obj* out_names = KEEP(rray_reduce_names(x, axes));

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(7);
  return out;
}

static struct rray_strided_iterator_plan rray_reduce_axes_plan(
  const int* v_x_dimensions,
  const r_ssize* v_x_strides,
  const int* v_axes,
  r_ssize axes_size
) {
  int v_dimensions[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    v_dimensions[i] = v_x_dimensions[axis - 1];
    v_strides[i] = v_x_strides[axis - 1];
  }

  return rray_strided_iterator_plan(v_dimensions, (int) axes_size, v_strides);
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
