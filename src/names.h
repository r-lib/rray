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

// Broadcasting of array names
//
// Assumes the inputs are validated arrays and are broadcastable to the
// `dimensions`. The caller typically checks all of this already.
r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions);
r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions);
r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions);

#endif
