#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"
#include "strided-iterator2.h"

#define RRAY_REDUCE_ARGS(...) , __VA_ARGS__
#define RRAY_REDUCE_NO_ARGS

// --------------------------------------------------------------------------
// rray_reduce

// Flat reduction is useful when the output element is the only state carried
// between input elements. It walks `x` once in storage order, updating the
// output element that each input contributes to. This is faster than nested
// reduction because it reads `x` linearly and avoids a separate inner traversal
// for each output element.

typedef r_obj* (*rray_reduce_fn)(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
);

typedef rray_reduce_fn (*rray_reduce_fn_switch)(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_reduce(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

typedef r_obj* (*rray_reduce_run_fn)(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);

typedef rray_reduce_run_fn (*rray_reduce_run_fn_switch)(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_reduce_run(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_run_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#define RRAY_REDUCE(                                                           \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  OUT_INIT,                                                                    \
  ONE,                                                                         \
  ONE_ARGS                                                                     \
)                                                                              \
  const r_ssize size = rray_strided_iterator_plan_size(plan);                  \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    v_out[i] = OUT_INIT;                                                       \
  }                                                                            \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  r_ssize run_start = 0;                                                       \
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);          \
                                                                               \
  r_ssize out_start = 0;                                                       \
  const r_ssize out_run_stride = rray_strided_iterator_plan_run_stride(plan);  \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  rray_strided_iterator_plan_point_init(plan, v_point);                        \
                                                                               \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize out_loc = out_start;                                               \
                                                                               \
    if (out_run_stride == 0) {                                                 \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i] ONE_ARGS);                 \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i] ONE_ARGS);                 \
        out_loc += out_run_stride;                                             \
      }                                                                        \
    }                                                                          \
                                                                               \
    run_start = run_end;                                                       \
    RRAY_STRIDED_ITERATOR_NEXT(out_start, v_point, plan);                      \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_REDUCE_RUN(                                                       \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  OUT_INIT,                                                                    \
  ONE,                                                                         \
  ONE_ARGS                                                                     \
)                                                                              \
  struct rray_run_iterator it =                                                \
    rray_run_iterator1(v_dimensions, dimensionality, v_out_broadcast_strides); \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    v_out[i] = OUT_INIT;                                                       \
  }                                                                            \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {         \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    r_ssize out_loc = rray_run_iterator_loc(&it, 0);                           \
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);               \
                                                                               \
    if (out_stride == 0) {                                                     \
      for (r_ssize i = start; i < end; ++i) {                                  \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i] ONE_ARGS);                 \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = start; i < end; ++i) {                                  \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i] ONE_ARGS);                 \
        out_loc += out_stride;                                                 \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

// --------------------------------------------------------------------------
// rray_reduce_nested

// Nested reduction is needed when an output element cannot hold all reduction
// state, such as the sum and count required by a mean. The outer plan walks the
// retained axes to select each output element, and the inner plan walks the
// reduction axes to compute it. Conceptually, this is like using
// `rray_permute_axes()` to bring the reduction axes to the front, without
// materializing the permuted array.

typedef r_obj* (*rray_reduce_nested_fn)(
  r_obj* x,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan,
  struct r_lazy error_call
);

typedef rray_reduce_nested_fn (*rray_reduce_nested_fn_switch)(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_reduce_nested(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_nested_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#define RRAY_REDUCE_OUTER(                                                     \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE,                                                                         \
  ONE_ARGS                                                                     \
)                                                                              \
  const r_ssize size = rray_strided_iterator_plan_size(outer_plan);            \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  r_ssize out_run_start = 0;                                                   \
  const r_ssize out_run_size =                                                 \
    rray_strided_iterator_plan_run_size(outer_plan);                           \
                                                                               \
  r_ssize x_start = 0;                                                         \
  const r_ssize x_run_stride =                                                 \
    rray_strided_iterator_plan_run_stride(outer_plan);                         \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  rray_strided_iterator_plan_point_init(outer_plan, v_point);                  \
                                                                               \
  while (out_run_start != size) {                                              \
    const r_ssize out_run_end = out_run_start + out_run_size;                  \
    r_ssize x_loc = x_start;                                                   \
                                                                               \
    for (r_ssize i = out_run_start; i < out_run_end; ++i) {                    \
      v_out[i] = ONE(v_x, x_loc, inner_plan ONE_ARGS);                         \
      x_loc += x_run_stride;                                                   \
    }                                                                          \
                                                                               \
    out_run_start = out_run_end;                                               \
    RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, outer_plan);                  \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_REDUCE_INNER(X_CTYPE, ACCUMULATE)                                 \
  do {                                                                         \
    const r_ssize size = rray_strided_iterator_plan_size(inner_plan);          \
                                                                               \
    r_ssize run_start = 0;                                                     \
    const r_ssize run_size = rray_strided_iterator_plan_run_size(inner_plan);  \
                                                                               \
    const r_ssize x_run_stride =                                               \
      rray_strided_iterator_plan_run_stride(inner_plan);                       \
                                                                               \
    r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                  \
    rray_strided_iterator_plan_point_init(inner_plan, v_point);                \
                                                                               \
    while (run_start != size) {                                                \
      const r_ssize run_end = run_start + run_size;                            \
      r_ssize x_loc = x_start;                                                 \
                                                                               \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        ACCUMULATE;                                                            \
        x_loc += x_run_stride;                                                 \
      }                                                                        \
                                                                               \
      run_start = run_end;                                                     \
      RRAY_STRIDED_ITERATOR_NEXT(x_start, v_point, inner_plan);                \
    }                                                                          \
  } while (0)

// --------------------------------------------------------------------------
// stop_unsupported_reduce

r_no_return void stop_unsupported_reduce(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
