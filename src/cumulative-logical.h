#ifndef RRAY_CUMULATIVE_LOGICAL_H
#define RRAY_CUMULATIVE_LOGICAL_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cumulative_all(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_cumulative_any(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
