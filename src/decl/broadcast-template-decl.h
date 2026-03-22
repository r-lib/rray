#ifndef RRAY_BROADCAST_TEMPLATE_DECL_H
#define RRAY_BROADCAST_TEMPLATE_DECL_H

#include "rlang.h"

r_obj* rray_broadcast_dimension_names(
  r_obj* x_dimension_names,
  const int* v_x_dimension_sizes,
  r_ssize x_dimensionality,
  const int* v_dimension_sizes,
  r_ssize dimensionality
);

r_obj* rray_broadcast_dimension_titles(
  r_obj* x_dimension_titles,
  r_ssize x_dimensionality,
  r_ssize dimensionality
);

#endif
