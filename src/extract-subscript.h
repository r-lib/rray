#ifndef RRAY_EXTRACT_SUBSCRIPT_H
#define RRAY_EXTRACT_SUBSCRIPT_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_as_extract_subscript(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

#endif
