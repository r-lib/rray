#ifndef RRAY_EXTRACT_ASSIGN_H
#define RRAY_EXTRACT_ASSIGN_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_extract_assign(
  r_obj* x,
  r_obj* i,
  r_obj* value,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct rray_arg* value_arg,
  struct r_lazy error_call
);

#endif
