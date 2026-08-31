#ifndef RRAY_NAMES_DECL_H
#define RRAY_NAMES_DECL_H

#include "rlang.h"

static inline bool axis_is_reduced(
  int axis,
  const int* v_axes,
  r_ssize axes_size
);

static inline bool any_axis_has_names(r_obj* const* v_names, int dimensionality);

static inline void check_axis_names(
  r_obj* names,
  int axis,
  int dimension,
  struct r_lazy error_call
);

#endif
