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
  r_obj* const* v_froms,
  r_ssize froms_size,
  int to_dimensionality,
  r_ssize* v_out
) {
  for (r_ssize i = 0; i < froms_size; ++i) {
    r_obj* from_dimensions = r_dim(v_froms[i]);
    const int* v_from_dimensions = r_int_cbegin(from_dimensions);
    const int from_dimensionality = r_length(from_dimensions);

    r_ssize stride = 1;

    for (int axis = 0; axis < to_dimensionality; ++axis) {
      const int dimension =
        axis < from_dimensionality ? v_from_dimensions[axis] : 1;
      v_out[(r_ssize) axis * froms_size + i] = dimension == 1 ? 0 : stride;
      stride *= dimension;
    }
  }
}
