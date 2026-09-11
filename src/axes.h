#ifndef RRAY_AXES_H
#define RRAY_AXES_H

#include "rlang.h"

#include "arg.h"

r_obj* arg_as_axes(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* arg_as_permutation(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

void check_axis(
  int axis,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_axes_complement(
  const int* v_axes,
  r_ssize axes_size,
  int dimensionality
);

#endif
