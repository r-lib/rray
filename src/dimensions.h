#ifndef RRAY_DIMENSIONS_H
#define RRAY_DIMENSIONS_H

#include "rlang.h"

r_obj* rray_dimensions(r_obj* x, struct r_lazy error_call);

r_ssize rray_dimension(r_obj* x, r_ssize axis, struct r_lazy error_call);

r_obj* rray_set_dimensions(
  r_obj* x,
  r_obj* dimensions,
  struct r_lazy error_call
);

bool rray_dimensions_are_equal(
  const int* v_x_dimensions,
  r_ssize x_dimensionality,
  const int* v_y_dimensions,
  r_ssize y_dimensionality
);

r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct r_lazy error_call
);

r_obj* arg_as_dimensions(
  r_obj* dimensions,
  r_obj* arg,
  struct r_lazy error_call
);

#endif
