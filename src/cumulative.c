#include "cumulative.h"

#include "axes.h"
#include "dimensionality.h"
#include "size.h"
#include "utils.h"

r_obj* rray_cumulative(
  r_obj* x,
  int axis,
  rray_cumulative_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* dimensions = r_dim(x);
  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);
  check_dimensionality(dimensionality);

  check_axis(axis, dimensionality, rray_args.axis, error_call);

  const rray_cumulative_fn fn = fn_switch(x, arg, error_call);

  const r_ssize axis_dimension = v_dimensions[axis - 1];
  const r_ssize stride =
    r_length(x) == 0 ? 0 : rray_size_from_dimensions(v_dimensions, axis - 1);

  r_obj* out = KEEP(fn(x, stride, axis_dimension, error_call));
  r_attrib_poke_dim(out, dimensions);

  r_obj* names = r_dim_names(x);

  if (names != r_null) {
    r_attrib_poke_dim_names(out, names);
  }

  FREE(2);
  return out;
}
