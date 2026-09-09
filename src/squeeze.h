#ifndef RRAY_SQUEEZE_H
#define RRAY_SQUEEZE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_squeeze(
  r_obj* x,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
