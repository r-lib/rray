#ifndef RRAY_SUM_H
#define RRAY_SUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_sum(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

#endif
