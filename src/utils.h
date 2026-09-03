#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

#include "arg.h"

void check_unclassed(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_obj* arg_as_array(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

enum r_type arg_as_ptype(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

bool r_has_name_at(r_obj* names, r_ssize i);

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg
);

#endif
