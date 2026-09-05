#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "iterator.h"

typedef r_obj* (*rray_binary_arithmetic_core_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);

typedef rray_binary_arithmetic_core_fn (*rray_binary_arithmetic_switch_fn)(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_binary_arithmetic(
  r_obj* x,
  r_obj* y,
  rray_binary_arithmetic_switch_fn fn_switch,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_no_return void stop_unsupported_arithmetic(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#define RRAY_ARITHMETIC(                                                       \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_AS_OUT,                                                                    \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_AS_OUT,                                                                    \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE                                                                          \
)                                                                              \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  for (r_ssize i = 0; i < size; ++i) {                                         \
    v_out[i] = ONE(                                                            \
      X_AS_OUT(v_x[rray_iterator2_location1(it)]),                             \
      Y_AS_OUT(v_y[rray_iterator2_location2(it)]),                             \
      error_call                                                               \
    );                                                                         \
    rray_iterator2_next(it);                                                   \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
