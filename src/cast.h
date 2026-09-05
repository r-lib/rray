#ifndef RRAY_CAST_H
#define RRAY_CAST_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
);

#endif
