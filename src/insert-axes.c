#include "insert-axes.h"

#include "axes.h"
#include "dimensionality.h"
#include "dimensions.h"
#include "utils.h"
#include "wrapper.h"

r_obj* ffi_rray_insert_axes(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_insert_axes(ffi_x, ffi_axes, rray_args.x, error_call);
}

r_obj* rray_insert_axes(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  r_obj* x_dimensions = KEEP(rray_dimensions(x, arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  axes = KEEP(arg_as_bare_integer(axes, rray_args.axes, error_call));
  const r_ssize axes_size = r_length(axes);

  const int out_dimensionality =
    int_add_checked(dimensionality, (int) axes_size);
  check_max_dimensionality(out_dimensionality);

  check_axes(axes, out_dimensionality, rray_args.axes, error_call);
  const int* v_axes = r_int_cbegin(axes);

  r_obj* out_dimensions = KEEP(r_alloc_integer(out_dimensionality));
  int* v_out_dimensions = r_int_begin(out_dimensions);

  r_obj* x_names = r_dim_names(x);
  r_obj* const* v_x_names = x_names == r_null ? NULL : r_list_cbegin(x_names);

  r_obj* out_names = r_null;
  r_keep_loc out_names_loc;
  KEEP_HERE(out_names, &out_names_loc);

  r_ssize axes_i = 0;
  r_ssize x_i = 0;

  for (int i = 0; i < out_dimensionality; ++i) {
    if (axes_i < axes_size && v_axes[axes_i] - 1 == i) {
      v_out_dimensions[i] = 1;
      ++axes_i;
      continue;
    }

    v_out_dimensions[i] = v_x_dimensions[x_i];

    if (v_x_names != NULL && v_x_names[x_i] != r_null) {
      if (out_names == r_null) {
        out_names = r_alloc_list(out_dimensionality);
        KEEP_AT(out_names, out_names_loc);
      }

      r_list_poke(out_names, i, v_x_names[x_i]);
    }

    ++x_i;
  }

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim(out, out_dimensions);

  if (out_names != r_null) {
    r_attrib_poke_dim_names(out, out_names);
  }

  FREE(6);
  return out;
}
