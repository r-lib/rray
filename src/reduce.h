#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

r_obj* rray_reduce_dimension_sizes(
  const int* v_dimension_sizes,
  r_ssize dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

r_obj* rray_reduce_dimension_names(
  r_obj* const* v_dimension_names,
  r_ssize dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

#endif
