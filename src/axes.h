#ifndef RRAY_AXES_H
#define RRAY_AXES_H

#include "rlang.h"

r_obj* arg_as_axes(
  r_obj* axes,
  r_ssize dimensionality,
  r_obj* arg,
  struct r_lazy error_call
);

r_ssize arg_as_axis(
  r_obj* axis,
  r_ssize dimensionality,
  r_obj* arg,
  struct r_lazy error_call
);

#endif
