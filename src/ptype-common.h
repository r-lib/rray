#ifndef RRAY_PTYPE_COMMON_H
#define RRAY_PTYPE_COMMON_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_ptype_common(
  r_obj* xs,
  r_obj* ptype,
  struct rray_arg* p_arg,
  struct rray_arg* p_ptype_arg,
  struct r_lazy error_call
);

#endif
