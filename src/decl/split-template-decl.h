#ifndef RRAY_SPLIT_TEMPLATE_DECL_H
#define RRAY_SPLIT_TEMPLATE_DECL_H

#include "rlang.h"

void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  const int* v_out_dimensions,
  int dimensionality,
  r_ssize out_size
);

r_obj* rray_split_dimensions(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

#endif
