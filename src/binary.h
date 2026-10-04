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
  struct rray_run_iterator it = rray_run_iterator2(                            \
    v_dimensions,                                                              \
    dimensionality,                                                            \
    v_x_broadcast_strides,                                                     \
    v_y_broadcast_strides                                                      \
  );                                                                           \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, rray_run_iterator_size(&it)));   \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next2(&it)) {         \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    r_ssize x_loc = rray_run_iterator_loc(&it, 0);                             \
    const r_ssize x_stride = rray_run_iterator_stride(&it, 0);                 \
                                                                               \
    r_ssize y_loc = rray_run_iterator_loc(&it, 1);                             \
    const r_ssize y_stride = rray_run_iterator_stride(&it, 1);                 \
                                                                               \
    if (x_stride == 0 && y_stride == 0) {                                      \
      const X_CTYPE x_elt = v_x[x_loc];                                        \
      const Y_CTYPE y_elt = v_y[y_loc];                                        \
      for (r_ssize i = start; i < end; ++i) {                                  \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
      }                                                                        \
    } else if (x_stride == 0) {                                                \
      const X_CTYPE x_elt = v_x[x_loc];                                        \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
        y_loc += y_stride;                                                     \
      }                                                                        \
    } else if (y_stride == 0) {                                                \
      const Y_CTYPE y_elt = v_y[y_loc];                                        \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
        x_loc += x_stride;                                                     \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        const Y_CTYPE y_elt = v_y[y_loc];                                      \
        v_out[i] = ONE(X_CAST(x_elt), Y_CAST(y_elt) ONE_ARGS);                 \
        x_loc += x_stride;                                                     \
        y_loc += y_stride;                                                     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
