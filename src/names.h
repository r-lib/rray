#ifndef RRAY_NAMES_H
#define RRAY_NAMES_H

#include "rlang.h"

r_obj* rray_names(r_obj* x, struct r_lazy error_call);

r_obj* rray_axis_names(r_obj* x, int axis, struct r_lazy error_call);

r_obj* rray_set_names(r_obj* x, r_obj* names, struct r_lazy error_call);

r_obj* rray_set_axis_names(
  r_obj* x,
  int axis,
  r_obj* names,
  struct r_lazy error_call
);

r_obj* rray_broadcast_names(
  r_obj* const* v_names,
  const int* v_dimensions,
  int dimensionality,
  const int* v_out_dimensions,
  int out_dimensionality
);

r_obj* rray_reduce_names(
  r_obj* const* v_names,
  int dimensionality,
  const int* v_axes,
  r_ssize axes_size
);

void rray_split_names(
  r_obj* out,
  r_obj* const* v_x_names,
  const int* v_out_dimensions,
  int dimensionality,
  r_ssize out_size
);

#endif
