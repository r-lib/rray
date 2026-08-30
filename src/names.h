#ifndef RRAY_NAMES_H
#define RRAY_NAMES_H

#include "rlang.h"

r_obj* rray_names(r_obj* x, struct r_lazy error_call);

r_obj* rray_axis_names(r_obj* x, r_ssize axis, struct r_lazy error_call);

r_obj* rray_set_names(r_obj* x, r_obj* names, struct r_lazy error_call);

r_obj* rray_set_axis_names(
  r_obj* x,
  r_ssize axis,
  r_obj* names,
  struct r_lazy error_call
);

#endif
