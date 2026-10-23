#ifndef RRAY_CUMULATIVE_SUM_H
#define RRAY_CUMULATIVE_SUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cumulative_sum(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
