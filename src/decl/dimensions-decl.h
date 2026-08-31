#ifndef RRAY_DIMENSIONS_DECL_H
#define RRAY_DIMENSIONS_DECL_H

#include "rlang.h"

static inline void rray_dimensions2(
  int* v_out_dimensions,
  int* p_out_dimensionality,
  const int* v_x_dimensions,
  int x_dimensionality,
  struct r_lazy error_call
);

#endif
