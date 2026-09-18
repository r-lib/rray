#ifndef RRAY_BROADCAST_NAMES_H
#define RRAY_BROADCAST_NAMES_H

#include "rlang.h"

r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions);
r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions);
r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions);

r_obj* rray_broadcast_names_common_opts(
  r_obj* xs,
  r_obj* dimensions,
  const int* v_ignore,
  r_ssize ignore_size
);

#endif
