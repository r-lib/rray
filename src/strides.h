#ifndef RRAY_STRIDES_H
#define RRAY_STRIDES_H

#include "rlang.h"

void rray_fill_strides_from_dimensions(
  const int* v_dimensions,
  int dimensionality,
  r_ssize* v_out
);

void rray_fill_broadcast_strides_from_dimensions(
  const int* v_from_dimensions,
  int from_dimensionality,
  int to_dimensionality,
  r_ssize* v_out
);

#endif
