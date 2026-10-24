#ifndef RRAY_UNSTACK_H
#define RRAY_UNSTACK_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_unstack(
  r_obj* x,
  r_obj* axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
