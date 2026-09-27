#ifndef RRAY_SLICE_H
#define RRAY_SLICE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_slice(
  r_obj* x,
  r_obj* indices,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
);

#endif
