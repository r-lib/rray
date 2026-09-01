#include "reduce.h"

r_obj* rray_reduce_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
) {
  r_obj* out = KEEP(r_alloc_integer(dimensionality));
  int* v_out = r_int_begin(out);

  // Start with `v_dimensions`
  r_memcpy(v_out, v_dimensions, sizeof(int) * dimensionality);

  // Set `axes` to 1
  for (r_ssize i = 0; i < axes_size; ++i) {
    v_out[v_axes[i] - 1] = 1;
  }

  FREE(1);
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

r_obj* rray_reduce_names(
  r_obj* const* v_names,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
) {
  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < dimensionality; ++i) {
    if (v_names[i] == r_null) {
      // `out` stays `r_null` when there were no names before
      continue;
    }
    if (axis_is_reduced(i, v_axes, axes_size)) {
      // `out` is "cleared" to `r_null` when an axis is reduced
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }
    r_list_poke(out, i, v_names[i]);
  }

  FREE(1);
  return out;
}
