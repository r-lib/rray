#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

#include "arg.h"
#include "type.h"

void check_unclassed(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

r_obj* arg_as_array(r_obj* x, struct rray_arg* p_arg, struct r_lazy error_call);

enum rray_type arg_as_type(
  r_obj* x,
  struct rray_arg* p_arg,
  struct r_lazy error_call
);

int arg_as_int(r_obj* x, struct rray_arg* p_arg, struct r_lazy error_call);

bool r_has_name_at(r_obj* names, r_ssize i);

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* p_x_arg,
  struct rray_arg* p_to_arg
);

#endif
