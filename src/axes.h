#ifndef RRAY_AXES_H
#define RRAY_AXES_H

#include "rlang.h"

r_obj* arg_as_axes(
  r_obj* axes,
  r_ssize dimensionality,
  struct r_lazy error_call
);

#endif
