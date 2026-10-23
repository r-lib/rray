#ifndef RRAY_AXES_H
#define RRAY_AXES_H

#include "rlang.h"

#include "arg.h"

int arg_as_axis(
  r_obj* axis,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_unsorted(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_unsorted_integer(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_permutation(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* axes_normalize(
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

#endif
