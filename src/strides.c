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
