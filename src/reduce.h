#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

r_obj* rray_reduce_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

#endif
