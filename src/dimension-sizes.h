#ifndef RRAY_DIMENSION_SIZES_H
#define RRAY_DIMENSION_SIZES_H

#include "rlang.h"

r_obj* rray_dimension_sizes(r_obj* x, struct r_lazy error_call);

bool rray_dimension_sizes_are_equal(
  const int* v_x_dimension_sizes,
  r_ssize x_dimensionality,
  const int* v_y_dimension_sizes,
  r_ssize y_dimensionality
);

void check_dimension_sizes(r_obj* dimension_sizes, struct r_lazy error_call);

#endif
