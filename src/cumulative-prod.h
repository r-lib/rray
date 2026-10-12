#ifndef RRAY_CUMULATIVE_PROD_H
#define RRAY_CUMULATIVE_PROD_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cumulative_prod(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
