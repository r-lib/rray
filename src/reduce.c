#include "reduce.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "reduce-names.h"
#include "size.h"
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
  const r_ssize x_size =
    rray_size_from_dimensions(v_x_dimensions, dimensionality);

  struct rray_iterator it;
  rray_iterator_init(
    &it,
    v_x_dimensions,
    dimensionality,
    v_out_dimensions,
    dimensionality,
    x_size
  );

  const rray_reduce_fn fn = fn_switch(x, na_rm, arg, error_call);

  r_obj* out = KEEP(fn(x, out_size, &it));
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
