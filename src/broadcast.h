#ifndef RRAY_BROADCAST_H
#define RRAY_BROADCAST_H

#include "rlang.h"

r_obj* rray_broadcast(
  r_obj* x,
  r_obj* dimension_sizes,
  struct r_lazy error_call
);

void check_broadcastable(
  const int* v_x_dimension_sizes,
  r_ssize x_dimensionality,
  const int* v_dimension_sizes,
  r_ssize dimensionality,
  struct r_lazy error_call
);

#endif
