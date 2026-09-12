#ifndef RRAY_STRIDES_H
#define RRAY_STRIDES_H

#include "rlang.h"

void rray_fill_strides_from_dimensions(
  const int* v_dimensions,
  int dimensionality,
  r_ssize* v_out
);

#endif
