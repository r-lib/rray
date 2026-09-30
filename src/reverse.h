#ifndef RRAY_REVERSE_H
#define RRAY_REVERSE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_reverse(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

#endif
