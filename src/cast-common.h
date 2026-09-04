#ifndef RRAY_CAST_COMMON_H
#define RRAY_CAST_COMMON_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_cast_common(
  r_obj* xs,
  r_obj* to,
  struct rray_arg* arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
);

#endif
