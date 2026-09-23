#ifndef RRAY_SIZE_H
#define RRAY_SIZE_H

#include "rlang.h"

#include "arg.h"

r_ssize rray_size(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_ssize rray_size_from_dimensions(const int* v_dimensions, int dimensionality);

r_ssize rray_size_from_dimensions_checked(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);

void check_size_from_dimensions(r_obj* dimensions, struct r_lazy error_call);

#endif
