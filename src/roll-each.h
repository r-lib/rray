#ifndef RRAY_ROLL_EACH_H
#define RRAY_ROLL_EACH_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_roll_each(
  r_obj* x,
  r_obj* n,
  r_obj* axis,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

#endif
