#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

// --------------------------------------------------------------------------
// rray_reduce

typedef r_obj* (*rray_reduce_fn)(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
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

#define RRAY_REDUCE(                                                           \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  OUT_INIT,                                                                    \
  ONE                                                                          \
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
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);       \
                                                                               \
  while (run_start != size) {                                                  \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize out_loc = out_start;                                               \
                                                                               \
    if (out_run_stride == 0) {                                                 \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i]);                          \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[out_loc] = ONE(v_out[out_loc], v_x[i]);                          \
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

// --------------------------------------------------------------------------
// rray_reduce_grouped

typedef r_obj* (*rray_reduce_grouped_fn)(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* outer_plan,
  const struct rray_strided_iterator_plan* inner_plan
);

typedef rray_reduce_grouped_fn (*rray_reduce_grouped_fn_switch)(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_reduce_grouped(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce_grouped_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#define RRAY_REDUCE_OUTER(                                                     \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  ONE                                                                          \
)                                                                              \
  const r_ssize size = rray_strided_iterator_plan_size(outer_plan);            \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  r_ssize out_start = 0;                                                       \
  const r_ssize out_run_size =                                                 \
    rray_strided_iterator_plan_run_size(outer_plan);                           \
                                                                               \
  r_ssize x_start = 0;                                                         \
  const r_ssize x_run_stride =                                                 \
    rray_strided_iterator_plan_run_stride(outer_plan);                         \
                                                                               \
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                    \
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) outer_plan->dimensionality); \
                                                                               \
  while (out_start != size) {                                                  \
    const r_ssize out_run_end = out_start + out_run_size;                      \
    r_ssize x_base = x_start;                                                  \
                                                                               \
    for (r_ssize i = out_start; i < out_run_end; ++i) {                        \
      v_out[i] = ONE(v_x, x_base, inner_plan);                                 \
      x_base += x_run_stride;                                                  \
    }                                                                          \
                                                                               \
    out_start = out_run_end;                                                   \
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
    r_ssize x_start = x_base;                                                  \
    const r_ssize x_run_stride =                                               \
      rray_strided_iterator_plan_run_stride(inner_plan);                       \
                                                                               \
    r_ssize v_point[RRAY_MAX_DIMENSIONALITY];                                  \
    r_memset(                                                                  \
      v_point,                                                                 \
      0,                                                                       \
      sizeof(r_ssize) * (size_t) inner_plan->dimensionality                    \
    );                                                                         \
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
