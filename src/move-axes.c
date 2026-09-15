#include "move-axes.h"

#include "axes.h"
#include "dimensionality.h"
#include "permute-axes.h"

r_obj* ffi_rray_move_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_to,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_move_axes(ffi_x, ffi_axes, ffi_to, rray_args.x, error_call);
}

r_obj* rray_move_axes(
  r_obj* x,
  r_obj* axes,
  r_obj* to,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_max_dimensionality(dimensionality);

  axes = KEEP(
    arg_as_axes_unsorted(axes, dimensionality, rray_args.axes, error_call)
  );
  to = KEEP(arg_as_axes_unsorted(to, dimensionality, rray_args.to, error_call));

  const r_ssize axes_size = r_length(axes);
  const r_ssize to_size = r_length(to);

  if (axes_size != to_size) {
    r_abort_lazy_call(
      error_call,
      "`axes` (%" R_PRIdXLEN_T ") and `to` (%" R_PRIdXLEN_T
      ") must be the same length.",
      axes_size,
      to_size
    );
  }

  const int* v_axes = r_int_cbegin(axes);
  const int* v_to = r_int_cbegin(to);

  r_obj* permutation = KEEP(r_alloc_integer(dimensionality));
  int* v_permutation = r_int_begin(permutation);

  bool v_moved[RRAY_MAX_DIMENSIONALITY] = {false};
  bool v_filled[RRAY_MAX_DIMENSIONALITY] = {false};

  for (r_ssize i = 0; i < axes_size; ++i) {
    const int axis = v_axes[i];
    const int location = v_to[i];

    v_permutation[location - 1] = axis;
    v_moved[axis - 1] = true;
    v_filled[location - 1] = true;
  }

  int axis = 1;

  for (int i = 0; i < dimensionality; ++i) {
    if (v_filled[i]) {
      continue;
    }

    while (v_moved[axis - 1]) {
      ++axis;
    }

    v_permutation[i] = axis;
    ++axis;
  }

  r_obj* out = KEEP(rray_permute_axes(x, permutation, arg, error_call));

  FREE(4);
  return out;
}
