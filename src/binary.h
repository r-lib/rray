#ifndef RRAY_BINARY_H
#define RRAY_BINARY_H

#include "rlang.h"

#include "arg.h"
#include "iterator.h"

r_no_return void stop_unsupported_binary(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

#define RRAY_BINARY(                                                           \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE,                                                                         \
  DATA                                                                         \
)                                                                              \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  RRAY_ITERATOR2_FOR_EACH(it, i, x_loc, y_loc, {                               \
    v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc]), DATA);              \
  });                                                                          \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
