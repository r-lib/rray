#ifndef RRAY_REP_H
#define RRAY_REP_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
  int axis,
  bool each,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
