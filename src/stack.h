#ifndef RRAY_STACK_H
#define RRAY_STACK_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_stack(
  r_obj* xs,
  int axis,
  r_obj* ptype,
  struct rray_arg* arg,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
);

#endif
