#include "strides.h"

r_obj* rray_strides_from_dimensions(
  const int* v_dimensions,
  int dimensionality
) {
  r_obj* out = KEEP(r_alloc_raw(dimensionality * sizeof(r_ssize)));
  r_ssize* v_out = (r_ssize*) r_raw_begin(out);

  r_ssize stride = 1;

  for (int i = 0; i < dimensionality; ++i) {
    v_out[i] = stride;
    stride *= v_dimensions[i];
  }

  FREE(1);
  return out;
}
