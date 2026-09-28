#ifndef RRAY_ROLL_H
#define RRAY_ROLL_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_roll(
  r_obj* x,
  r_obj* n,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

#endif
