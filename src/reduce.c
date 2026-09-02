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
