#ifndef RRAY_BROADCAST_TEMPLATE_DECL_H
#define RRAY_BROADCAST_TEMPLATE_DECL_H

#include "rlang.h"

r_obj* rray_broadcast_dimension_names(
  r_obj* const* v_dimension_names,
  const int* v_dimensions,
  r_ssize dimensionality,
  const int* v_out_dimensions,
  r_ssize out_dimensionality
);

#endif
