#ifndef RRAY_BROADCAST_NAMES_H
#define RRAY_BROADCAST_NAMES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions);

r_obj* rray_broadcast_names2(
  r_obj* x,
  r_obj* y,
  r_obj* dimensions,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_broadcast_names_common_opts(
  r_obj* xs,
  r_obj* dimensions,
  const int* v_ignore,
  r_ssize ignore_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
