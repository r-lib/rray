#ifndef RRAY_IF_ELSE_H
#define RRAY_IF_ELSE_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_if_else(
  r_obj* condition,
  r_obj* true_,
  r_obj* false_,
  r_obj* missing,
  r_obj* dimensions,
  struct rray_arg* condition_arg,
  struct rray_arg* true_arg,
  struct rray_arg* false_arg,
  struct rray_arg* missing_arg,
  struct r_lazy error_call
);

#endif
