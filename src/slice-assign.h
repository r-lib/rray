#ifndef RRAY_SLICE_ASSIGN_H
#define RRAY_SLICE_ASSIGN_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_slice_assign(
  r_obj* x,
  r_obj* indices,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct rray_arg* value_arg,
  struct r_lazy error_call
);

#endif
