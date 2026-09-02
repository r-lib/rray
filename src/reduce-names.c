#include "reduce-names.h"

#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"

#include "decl/reduce-names-decl.h"

r_obj* ffi_rray_reduce_names(r_obj* ffi_x, r_obj* ffi_axes) {
  return rray_reduce_names(ffi_x, ffi_axes);
}

// Reduction of array names
//
// Assumes the inputs are validated arrays and that `axes` are valid axes of
// `x`. The caller typically checks all of this already.
//
// Mimics `rray_broadcast_names()` ideas, but reduction only ever has 1 input
r_obj* rray_reduce_names(r_obj* x, r_obj* axes) {
  r_obj* x_names = rray_names(x, rray_args.x, r_lazy_null);

  if (x_names == r_null) {
    return r_null;
  }

  KEEP(x_names);

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* x_dimensions = KEEP(rray_dimensions(x, rray_args.x, r_lazy_null));
  const int dimensionality = rray_dimensionality_from_dimensions(x_dimensions);

  const int* v_axes = r_int_cbegin(axes);
  const r_ssize axes_size = r_length(axes);

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    if (v_x_names[i] == r_null) {
      // No names to contribute on this axis
      continue;
    }
    if (axis_is_reduced(i, v_axes, axes_size)) {
      // No names to contribute when axis is reduced
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_x_names[i]);
  }

  FREE(3);
  return out;
}

static inline bool axis_is_reduced(
  int axis,
  const int* v_axes,
  r_ssize axes_size
) {
  for (r_ssize i = 0; i < axes_size; ++i) {
    if (v_axes[i] - 1 == axis) {
      return true;
    }
  }

  return false;
}
