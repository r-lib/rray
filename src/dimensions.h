#ifndef RRAY_DIMENSIONS_H
#define RRAY_DIMENSIONS_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_dimensions(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

int rray_dimension(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_dimensions(
  r_obj* x,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_axes_dimension(
  const int* v_dimensions,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size,
  int dimension
);

bool rray_dimensions_are_equal(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_y_dimensions,
  int y_dimensionality
);

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
);

r_obj* arg_as_dimensions(
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
