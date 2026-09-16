#ifndef RRAY_COMBINE_H
#define RRAY_COMBINE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_combine(
  r_obj* xs,
  int axis,
  r_obj* ptype,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
