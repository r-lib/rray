#ifndef RRAY_MOVE_AXES_H
#define RRAY_MOVE_AXES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_move_axes(
  r_obj* x,
  r_obj* from,
  r_obj* to,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
