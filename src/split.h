#ifndef RRAY_SPLIT_H
#define RRAY_SPLIT_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_split(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
