#ifndef RRAY_PTYPE_H
#define RRAY_PTYPE_H

#include "rlang.h"

#include "arg.h"

enum r_type rray_ptype2(
  enum r_type x,
  enum r_type y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

enum r_type rray_ptype_common(r_obj* xs, struct r_lazy error_call);

#endif
