#ifndef RRAY_SPLIT_TEMPLATE_DECL_H
#define RRAY_SPLIT_TEMPLATE_DECL_H

#include "rlang.h"

void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  r_ssize dimensionality,
  const int* v_x_dimensions,
  const int* v_axes,
  r_ssize axes_size,
  r_ssize n_splits
);

r_obj* rray_split_dimensions(
  const int* v_dimensions,
  r_ssize dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

#endif
