#ifndef RRAY_BINARY_H
#define RRAY_BINARY_H

#include "rlang.h"

#include "dimensionality.h"
#include "strided-iterator.h"
#include "strided-iterator2.h"
#include "strides.h"

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
  const r_ssize size = rray_strided_iterator2_plan_size(plan);                 \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  r_ssize run_start = 0;                                                       \
  const r_ssize run_size = rray_strided_iterator2_plan_run_size(plan);         \
                                                                               \
  r_ssize x_start = 0;                                                         \
  const r_ssize x_run_stride = rray_strided_iterator2_plan_run_stride1(plan);  \
                                                                               \
  r_ssize y_start = 0;                                                         \
  const r_ssize y_run_stride = rray_strided_iterator2_plan_run_stride2(plan);  \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  rray_strided_iterator2_plan_point_init(plan, v_point);                       \
                                                                               \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize x_loc = x_start;                                                   \
    r_ssize y_loc = y_start;                                                   \
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
                                                                               \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT2(x_start, y_start, v_point, plan);              \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

static inline struct rray_run_iterator rray_binary_run_iterator(
  const int* v_x_dimensions,
  int x_dimensionality,
  const int* v_y_dimensions,
  int y_dimensionality,
  const int* v_dimensions,
  int dimensionality
) {
  check_dimensionality(dimensionality);

  r_ssize v_x_broadcast_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_x_dimensions,
    x_dimensionality,
    dimensionality,
    v_x_broadcast_strides
  );

  r_ssize v_y_broadcast_strides[RRAY_MAX_DIMENSIONALITY];
  rray_fill_broadcast_strides_from_dimensions(
    v_y_dimensions,
    y_dimensionality,
    dimensionality,
    v_y_broadcast_strides
  );

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY * 2];
  for (int axis = 0; axis < dimensionality; ++axis) {
    v_strides[axis * 2] = v_x_broadcast_strides[axis];
    v_strides[axis * 2 + 1] = v_y_broadcast_strides[axis];
  }

  return rray_run_iterator(v_dimensions, dimensionality, v_strides, 2);
}

#define RRAY_BINARY_RUN(                                                       \
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
  struct rray_run_iterator it = rray_binary_run_iterator(                      \
    v_x_dimensions,                                                            \
    x_dimensionality,                                                          \
    v_y_dimensions,                                                            \
    y_dimensionality,                                                          \
    v_dimensions,                                                              \
    dimensionality                                                             \
  );                                                                           \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, rray_run_iterator_size(&it)));   \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);                                       \
                                                                               \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next(&it, 2)) {       \
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
