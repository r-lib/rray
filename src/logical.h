#ifndef RRAY_LOGICAL_H
#define RRAY_LOGICAL_H

#include "rlang.h"

#include "arg.h"
#include "type.h"

r_obj* rray_and(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_or(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_xor(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static inline void check_logical(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  switch (rray_typeof(x)) {
  case RRAY_TYPE_logical:
    return;

  case RRAY_TYPE_integer:
  case RRAY_TYPE_double:
  case RRAY_TYPE_complex:
  case RRAY_TYPE_character:
  case RRAY_TYPE_raw:
  case RRAY_TYPE_list:
  case RRAY_TYPE_scalar:
    r_abort_lazy_call(
      error_call,
      "%s must be a logical array, not %s.",
      rray_arg_format_input(arg),
      r_obj_type_friendly(x)
    );
  }

  r_stop_unreachable();
}

#endif
