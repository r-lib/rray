#ifndef RRAY_STRIDES_H
#define RRAY_STRIDES_H

#include "rlang.h"

r_obj* rray_strides_from_dimensions(
  const int* v_dimensions,
  int dimensionality
);

#endif
