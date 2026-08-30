#ifndef RRAY_NAMES_DECL_H
#define RRAY_NAMES_DECL_H

#include "rlang.h"

extern r_obj* row_axis;
extern r_obj* col_axis;

static inline r_obj* axis_names_impl(
  r_obj* x,
  r_obj* axis,
  struct r_lazy error_call
);

static inline r_obj* set_axis_names_impl(
  r_obj* x,
  r_obj* axis,
  r_obj* names,
  struct r_lazy error_call
);

static inline void check_axis_names(
  r_obj* names,
  r_ssize axis,
  r_ssize dimension,
  struct r_lazy error_call
);

#endif
