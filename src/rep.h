#ifndef RRAY_REP_H
#define RRAY_REP_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_no_return void stop_rep_dimension_too_large(struct r_lazy error_call);

#endif
