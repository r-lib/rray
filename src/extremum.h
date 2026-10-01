#ifndef RRAY_EXTREMUM_H
#define RRAY_EXTREMUM_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_pmax(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_pmin(
  r_obj* x,
  r_obj* y,
  bool na_rm,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#endif
