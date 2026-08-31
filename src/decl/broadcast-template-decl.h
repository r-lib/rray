#ifndef RRAY_BROADCAST_TEMPLATE_DECL_H
#define RRAY_BROADCAST_TEMPLATE_DECL_H

#include "rlang.h"

r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  int dimensionality,
  const int* v_out_dimensions,
  int out_dimensionality
);

#endif
