#ifndef RRAY_TRANSPOSE_H
#define RRAY_TRANSPOSE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_transpose(
  r_obj* x,
  r_obj* permutation,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
