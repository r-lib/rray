#include "strides.h"

void rray_fill_strides_from_dimensions(
  const int* v_dimensions,
  int dimensionality,
  r_ssize* v_out
) {
  r_ssize stride = 1;

  for (int i = 0; i < dimensionality; ++i) {
    v_out[i] = stride;
    stride *= v_dimensions[i];
  }
}

void rray_fill_broadcast_strides_from_dimensions(
  const int* v_from_dimensions,
  int from_dimensionality,
  int to_dimensionality,
  r_ssize* v_out
) {
  r_ssize stride = 1;

  for (int i = 0; i < to_dimensionality; ++i) {
    const int dimension = (i < from_dimensionality) ? v_from_dimensions[i] : 1;
    v_out[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }
}

void rray_fill_broadcast_strides_from_arrays(
  r_obj* const* v_xs,
  r_ssize xs_size,
  const int* v_dimensions,
  int dimensionality,
  r_ssize* v_out
) {
  for (r_ssize i = 0; i < xs_size; ++i) {
    r_obj* x_dimensions = r_dim(v_xs[i]);
    const int* v_x_dimensions = r_int_cbegin(x_dimensions);
    const int x_dimensionality = r_length(x_dimensions);

    r_ssize stride = 1;

    for (int axis = 0; axis < dimensionality; ++axis) {
      const int dimension = axis < x_dimensionality ? v_x_dimensions[axis] : 1;

      if (dimension != 1 && dimension != v_dimensions[axis]) {
        r_stop_internal("Array dimensions must be broadcast compatible.");
      }

      v_out[(r_ssize) axis * xs_size + i] = dimension == 1 ? 0 : stride;
      stride *= dimension;
    }
  }
}
