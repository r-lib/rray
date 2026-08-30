#ifndef RRAY_NAMES_DECL_H
#define RRAY_NAMES_DECL_H

#include "rlang.h"

extern r_obj* row_axis;
extern r_obj* col_axis;

static inline void check_axis_names(
  r_obj* names,
  r_ssize axis,
  int dimension,
  struct r_lazy error_call
);

#endif
