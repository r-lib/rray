#include "reduce.h"

r_obj* rray_reduce_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  int axes_size
) {
  r_obj* out = KEEP(r_alloc_integer(dimensionality));
  int* v_out = r_int_begin(out);

  // Start with `v_dimensions`
  memcpy(v_out, v_dimensions, sizeof(int) * dimensionality);

  // Set `axes` to 1
  for (int i = 0; i < axes_size; ++i) {
    v_out[v_axes[i] - 1] = 1;
  }

  FREE(1);
  return out;
}

r_obj* rray_reduce_names(
  r_obj* const* v_names,
  int dimensionality,
  const int* v_axes,
  int axes_size
) {
  int i = 0;

  for (; i < dimensionality; ++i) {
    r_obj* axis_names = v_names[i];

    // No names for this axis
    if (axis_names == r_null) {
      continue;
    }

    // If there are names for this axis, but we are reducing this
    // axis, then those names will be dropped and don't actually
    // count for our early exit criteria
    for (int j = 0; j < axes_size; ++j) {
      if (v_axes[j] - 1 == i) {
        axis_names = r_null;
        break;
      }
    }
    if (axis_names == r_null) {
      continue;
    }

    // Usable names for an axis, break
    break;
  }

  if (i == dimensionality) {
    // No names left after reducing
    return r_null;
  }

  // Everything up to `i` is `r_null`
  r_obj* out = KEEP(r_alloc_list(dimensionality));

  for (; i < dimensionality; ++i) {
    r_obj* axis_names = v_names[i];

    if (axis_names == r_null) {
      continue;
    }

    for (int j = 0; j < axes_size; ++j) {
      if (v_axes[j] - 1 == i) {
        axis_names = r_null;
        break;
      }
    }

    if (axis_names == r_null) {
      continue;
    }

    r_list_poke(out, i, axis_names);
  }

  FREE(1);
  return out;
}
