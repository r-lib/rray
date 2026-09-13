#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

typedef r_obj* (*rray_binary_arithmetic_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_strided_iterator2* it,
  struct r_lazy error_call
);

typedef rray_binary_arithmetic_fn (*rray_binary_arithmetic_switch_fn)(
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

r_no_return void stop_int_overflow(struct r_lazy error_call);

#define RRAY_ARITHMETIC(                                                       \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  X_CAST,                                                                      \
  Y_CTYPE,                                                                     \
  Y_CONST_DEREF,                                                               \
  Y_CAST,                                                                      \
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
  struct rray_strided_iterator2_cursor cursor =                                \
    rray_strided_iterator2_begin(it);                                          \
  for (; !rray_strided_iterator2_finished(&cursor);                            \
       rray_strided_iterator2_next(&cursor)) {                                 \
    const r_ssize index = rray_strided_iterator2_index(&cursor);               \
    const r_ssize end = index + rray_strided_iterator2_run_size(&cursor);      \
    r_ssize x_loc = rray_strided_iterator2_location1(&cursor);                 \
    r_ssize y_loc = rray_strided_iterator2_location2(&cursor);                 \
    const r_ssize x_stride = rray_strided_iterator2_run_stride1(&cursor);      \
    const r_ssize y_stride = rray_strided_iterator2_run_stride2(&cursor);      \
                                                                               \
    if (x_stride == 0) {                                                       \
      const X_CTYPE x_elt = v_x[x_loc];                                        \
      if (y_stride == 0) {                                                     \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        for (r_ssize i = index; i < end; ++i) {                                \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt), error_call);            \
        }                                                                      \
      } else {                                                                 \
        for (r_ssize i = index; i < end; ++i, y_loc += y_stride) {             \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(v_y[y_loc]), error_call);       \
        }                                                                      \
      }                                                                        \
    } else if (y_stride == 0) {                                                \
      const Y_CTYPE y_elt = v_y[y_loc];                                        \
      for (r_ssize i = index; i < end; ++i, x_loc += x_stride) {               \
        v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(y_elt), error_call);         \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = index; i < end;                                         \
           ++i, x_loc += x_stride, y_loc += y_stride) {                        \
        v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc]), error_call);    \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
