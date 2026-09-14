#ifndef RRAY_BINARY_H
#define RRAY_BINARY_H

#include "rlang.h"

#include "strided-iterator.h"

#define RRAY_BINARY_ARGS(...) , __VA_ARGS__
#define RRAY_BINARY_NO_ARGS

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
  ONE_ARGS                                                                     \
)                                                                              \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  const r_ssize run_size = rray_strided_iterator2_plan_run_size(plan);         \
  const r_ssize x_run_stride = rray_strided_iterator2_plan_run_stride1(plan);  \
  const r_ssize y_run_stride = rray_strided_iterator2_plan_run_stride2(plan);  \
                                                                               \
  for (struct rray_strided_iterator2 it = rray_strided_iterator2();            \
       !rray_strided_iterator2_finished(&it, plan);                            \
       rray_strided_iterator2_next(&it, plan)) {                               \
    const r_ssize run_start = rray_strided_iterator2_run_start(&it);           \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize x_loc = rray_strided_iterator2_location1(&it);                     \
    r_ssize y_loc = rray_strided_iterator2_location2(&it);                     \
                                                                               \
    if (x_run_stride == 0) {                                                   \
      const X_CTYPE x_elt = v_x[x_loc];                                        \
      if (y_run_stride == 0) {                                                 \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        for (r_ssize i = run_start; i < run_end; ++i) {                        \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);               \
        }                                                                      \
      } else {                                                                 \
        for (r_ssize i = run_start; i < run_end; ++i) {                        \
          const Y_CTYPE y_elt = v_y[y_loc];                                    \
          v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);               \
          y_loc += y_run_stride;                                               \
        }                                                                      \
      }                                                                        \
    } else if (y_run_stride == 0) {                                            \
      const Y_CTYPE y_elt = v_y[y_loc];                                        \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
        x_loc += x_run_stride;                                                 \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
        x_loc += x_run_stride;                                                 \
        y_loc += y_run_stride;                                                 \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
