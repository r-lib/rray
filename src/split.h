#ifndef RRAY_SPLIT_H
#define RRAY_SPLIT_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_split(
  r_obj* x,
  int axis,
  r_obj* sizes,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
