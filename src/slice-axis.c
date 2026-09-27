#include "slice-axis.h"

#include "axes.h"
#include "dimensionality.h"
#include "slice-assign.h"
#include "slice.h"
#include "utils.h"

#include "decl/slice-axis-decl.h"

r_obj* ffi_rray_slice_axis(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_axis,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_slice_axis(
    ffi_x,
    ffi_i,
    axis,
    rray_args.x,
    rray_args.i,
    error_call
  );
}

r_obj* ffi_rray_slice_rows(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_slice_axis(ffi_x, ffi_i, 1, rray_args.x, rray_args.i, error_call);
}

r_obj* ffi_rray_slice_columns(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_slice_axis(ffi_x, ffi_i, 2, rray_args.x, rray_args.i, error_call);
}

r_obj* rray_slice_axis(
  r_obj* x,
  r_obj* i,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  const int dimensionality = rray_dimensionality(x, x_arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  r_obj* indices =
    KEEP(rray_slice_axis_indices(i, axis, dimensionality, i_arg));

  r_obj* out = rray_slice(x, indices, x_arg, rray_args.empty, error_call);

  FREE(2);
  return out;
}

r_obj* ffi_rray_slice_assign_axis(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_axis,
  r_obj* ffi_value,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_slice_assign_axis(
    ffi_x,
    ffi_i,
    axis,
    ffi_value,
    rray_args.x,
    rray_args.i,
    rray_args.value,
    error_call
  );
}

r_obj* ffi_rray_slice_assign_rows(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_slice_assign_axis(
    ffi_x,
    ffi_i,
    1,
    ffi_value,
    rray_args.x,
    rray_args.i,
    rray_args.value,
    error_call
  );
}

r_obj* ffi_rray_slice_assign_columns(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_slice_assign_axis(
    ffi_x,
    ffi_i,
    2,
    ffi_value,
    rray_args.x,
    rray_args.i,
    rray_args.value,
    error_call
  );
}

r_obj* rray_slice_assign_axis(
  r_obj* x,
  r_obj* i,
  int axis,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct rray_arg* value_arg,
  struct r_lazy error_call
) {
  check_unclassed(x, x_arg, error_call);
  x = KEEP(arg_as_array(x, x_arg, error_call));

  const int dimensionality = rray_dimensionality(x, x_arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  r_obj* indices =
    KEEP(rray_slice_axis_indices(i, axis, dimensionality, i_arg));

  r_obj* out = rray_slice_assign(
    x,
    indices,
    value,
    x_arg,
    rray_args.empty,
    value_arg,
    error_call
  );

  FREE(2);
  return out;
}

static r_obj* rray_slice_axis_indices(
  r_obj* i,
  int axis,
  int dimensionality,
  struct rray_arg* i_arg
) {
  r_obj* out = KEEP(r_alloc_list(dimensionality));
  r_obj* names = KEEP(r_alloc_character(dimensionality));

  for (int j = 0; j < dimensionality; ++j) {
    r_list_poke(out, j, r_true);
  }

  r_list_poke(out, axis - 1, i);
  r_chr_poke(names, axis - 1, r_chr_get(rray_arg(i_arg), 0));

  r_attrib_poke_names(out, names);

  FREE(2);
  return out;
}
