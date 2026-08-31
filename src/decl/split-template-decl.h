#ifndef RRAY_SPLIT_TEMPLATE_DECL_H
#define RRAY_SPLIT_TEMPLATE_DECL_H

#include "rlang.h"

r_obj* rray_split_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

#endif
