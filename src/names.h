#ifndef RRAY_NAMES_H
#define RRAY_NAMES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_names(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_obj* rray_axis_names(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_names(
  r_obj* x,
  r_obj* names,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_axis_names(
  r_obj* x,
  int axis,
  r_obj* names,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_broadcast_names2(
  r_obj* x,
  r_obj* y,
  r_obj* dimensions,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_broadcast_names(
  r_obj* names,
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_dimensions,
  int dimensionality
);

#endif
