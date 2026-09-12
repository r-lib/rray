#ifndef RRAY_REDUCE_LOGICAL_H
#define RRAY_REDUCE_LOGICAL_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_all_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_any_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
