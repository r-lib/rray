#ifndef RRAY_DIMENSIONALITY_H
#define RRAY_DIMENSIONALITY_H

#include "rlang.h"

#include "arg.h"

#define RRAY_MAX_DIMENSIONALITY 64
#define RRAY_MAX_INPUTS 64

int rray_dimensionality(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
int rray_dimensionality_from_dimensions(r_obj* dimensions);

r_obj* rray_expand_dimensionality(
  r_obj* x,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

int list_max_dimensionality(
  r_obj* xs,
  struct rray_arg* arg,
  struct r_lazy error_call
);

void check_dimensionality(int dimensionality);

#endif
