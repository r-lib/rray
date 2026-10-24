#ifndef RRAY_REP_EACH_H
#define RRAY_REP_EACH_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_rep_each(
  r_obj* x,
  r_obj* times,
  r_obj* axis,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

#endif
