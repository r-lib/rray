#ifndef RRAY_CUMULATIVE_EXTREMUM_H
#define RRAY_CUMULATIVE_EXTREMUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cumulative_max(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_cumulative_min(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
