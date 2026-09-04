#ifndef RRAY_NAMES_H
#define RRAY_NAMES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_names(r_obj* x, struct rray_arg* p_arg, struct r_lazy error_call);

r_obj* rray_axis_names(
  r_obj* x,
  int axis,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

r_obj* rray_set_names(
  r_obj* x,
  r_obj* names,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

r_obj* rray_set_axis_names(
  r_obj* x,
  int axis,
  r_obj* names,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

#endif
