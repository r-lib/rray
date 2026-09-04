#ifndef RRAY_BROADCAST_H
#define RRAY_BROADCAST_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_broadcast(
  r_obj* x,
  r_obj* dimensions,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

r_obj* rray_broadcast_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
);

void check_broadcastable(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

#endif
