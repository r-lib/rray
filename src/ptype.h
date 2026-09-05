#ifndef RRAY_PTYPE_H
#define RRAY_PTYPE_H

#include "rlang.h"

#include "arg.h"
#include "type.h"
#include "typeof2.h"

r_obj* rray_ptype(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_obj* rray_ptype2(
  r_obj* x,
  r_obj* y,
  enum rray_side* side,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#endif
