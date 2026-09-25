#ifndef RRAY_EXTRACT_H
#define RRAY_EXTRACT_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_extract(
  r_obj* x,
  r_obj* i,
  struct rray_arg* x_arg,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

#endif
