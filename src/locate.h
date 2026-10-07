#ifndef RRAY_LOCATE_H
#define RRAY_LOCATE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_locate_max(
  r_obj* x,
  int axis,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_locate_min(
  r_obj* x,
  int axis,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
