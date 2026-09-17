#ifndef RRAY_REMOVE_AXES_H
#define RRAY_REMOVE_AXES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_remove_axes(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
