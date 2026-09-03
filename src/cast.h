#ifndef RRAY_CAST_H
#define RRAY_CAST_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cast(
  r_obj* x,
  enum r_type to,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_cast_common(r_obj* xs, enum r_type to, struct r_lazy error_call);

#endif
