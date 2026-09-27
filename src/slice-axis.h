#ifndef RRAY_SLICE_AXIS_H
#define RRAY_SLICE_AXIS_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_slice_axis(
  r_obj* x,
  r_obj* i,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

r_obj* rray_slice_assign_axis(
  r_obj* x,
  r_obj* i,
  int axis,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct rray_arg* value_arg,
  struct r_lazy error_call
);

#endif
