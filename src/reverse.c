#include "reverse.h"

#include "axes.h"
#include "dimensionality.h"
#include "slice.h"
#include "utils.h"

#include "decl/reverse-decl.h"

r_obj* ffi_rray_reverse(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_reverse(ffi_x, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_reverse(
  r_obj* x,
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

  r_obj* indices = KEEP(r_alloc_list(dimensionality));

  for (int i = 0; i < dimensionality; ++i) {
    r_list_poke(indices, i, r_true);
  }

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int dimension = v_x_dimensions[axis - 1];
    r_list_poke(indices, axis - 1, rray_reverse_locations(dimension));
  }

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(3);
  return out;
}

static r_obj* rray_reverse_locations(int dimension) {
  r_obj* out = KEEP(r_alloc_integer(dimension));
  int* v_out = r_int_begin(out);

  for (int i = 0; i < dimension; ++i) {
    v_out[i] = dimension - i;
  }

  FREE(1);
  return out;
}
