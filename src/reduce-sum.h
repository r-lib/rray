#ifndef RRAY_REDUCE_SUM_H
#define RRAY_REDUCE_SUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_sum_along(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
