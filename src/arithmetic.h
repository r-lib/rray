#ifndef RRAY_ARITHMETIC_H
#define RRAY_ARITHMETIC_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

typedef r_obj* (*rray_binary_arithmetic_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  const struct rray_strided_iterator2_plan* plan,
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
  const r_ssize run_size = rray_strided_iterator2_plan_run_size(plan);         \
  const r_ssize x_stride = rray_strided_iterator2_plan_run_stride1(plan);      \
  const r_ssize y_stride = rray_strided_iterator2_plan_run_stride2(plan);      \
                                                                               \
  for (struct rray_strided_iterator2 it = rray_strided_iterator2(plan);        \
       !rray_strided_iterator2_finished(&it);                                  \
       rray_strided_iterator2_next(&it)) {                                     \
    const r_ssize index = rray_strided_iterator2_index(&it);                   \
    const r_ssize end = index + run_size;                                      \
    r_ssize x_loc = rray_strided_iterator2_location1(&it);                     \
    r_ssize y_loc = rray_strided_iterator2_location2(&it);                     \
                                                                               \
    if (x_stride == 0) {                                                       \
      const X_CTYPE x_elt = v_x[x_loc];                                        \
      if (y_stride == 0) {                                                     \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        for (r_ssize i = index; i < end; ++i) {                                \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt), error_call);            \
        }                                                                      \
      } else {                                                                 \
        for (r_ssize i = index; i < end; ++i) {                                \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(v_y[y_loc]), error_call);       \
          y_loc += y_stride;                                                   \
        }                                                                      \
      }                                                                        \
    } else if (y_stride == 0) {                                                \
      const Y_CTYPE y_elt = v_y[y_loc];                                        \
      for (r_ssize i = index; i < end; ++i) {                                  \
        v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(y_elt), error_call);         \
        x_loc += x_stride;                                                     \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = index; i < end; ++i) {                                  \
        v_out[i] = ONE(X_CAST(v_x[x_loc]), Y_CAST(v_y[y_loc]), error_call);    \
        x_loc += x_stride;                                                     \
        y_loc += y_stride;                                                     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
