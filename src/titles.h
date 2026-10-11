#ifndef RRAY_TITLES_H
#define RRAY_TITLES_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_titles(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_obj* rray_axis_title(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_titles(
  r_obj* x,
  r_obj* titles,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_set_axis_title(
  r_obj* x,
  int axis,
  r_obj* title,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
