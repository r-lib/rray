#ifndef RRAY_CAPACITY_H
#define RRAY_CAPACITY_H

#include "rlang.h"

r_ssize rray_capacity(r_obj* x, struct r_lazy error_call);

R_xlen_t rray_capacity_from_dimension_sizes(
  const int* v_dimension_sizes,
  R_xlen_t dimensionality
);

#endif
