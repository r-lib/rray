#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

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
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
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
  struct rray_run_iterator it;                                                 \
  rray_run_iterator_init1(                                                     \
    &it,                                                                       \
    v_dimensions,                                                              \
    dimensionality,                                                            \
    v_out_broadcast_strides                                                    \
  );                                                                           \
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
  const int* v_outer_dimensions,
  int outer_dimensionality,
  const r_ssize* v_outer_strides,
  const int* v_inner_dimensions,
  int inner_dimensionality,
  const r_ssize* v_inner_strides,
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
  struct rray_run_iterator outer;                                              \
  rray_run_iterator_init1(                                                     \
    &outer,                                                                    \
    v_outer_dimensions,                                                        \
    outer_dimensionality,                                                      \
    v_outer_strides                                                            \
  );                                                                           \
                                                                               \
  struct rray_run_iterator inner;                                              \
  rray_run_iterator_init1(                                                     \
    &inner,                                                                    \
    v_inner_dimensions,                                                        \
    inner_dimensionality,                                                      \
    v_inner_strides                                                            \
  );                                                                           \
                                                                               \
  const r_ssize out_size = rray_run_iterator_size(&outer);                     \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  for (; !rray_run_iterator_done(&outer); rray_run_iterator_next1(&outer)) {   \
    const r_ssize start = rray_run_iterator_start(&outer);                     \
    const r_ssize end = rray_run_iterator_end(&outer);                         \
                                                                               \
    r_ssize x_loc = rray_run_iterator_loc(&outer, 0);                          \
    const r_ssize x_stride = rray_run_iterator_stride(&outer, 0);              \
                                                                               \
    for (r_ssize i = start; i < end; ++i) {                                    \
      v_out[i] = ONE(v_x, x_loc, &inner ONE_ARGS);                             \
      x_loc += x_stride;                                                       \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#define RRAY_REDUCE_INNER(X_CTYPE, ACCUMULATE)                                 \
  do {                                                                         \
    rray_run_iterator_reset1(inner);                                           \
                                                                               \
    for (; !rray_run_iterator_done(inner); rray_run_iterator_next1(inner)) {   \
      const r_ssize start = rray_run_iterator_start(inner);                    \
      const r_ssize end = rray_run_iterator_end(inner);                        \
                                                                               \
      r_ssize x_loc = x_start + rray_run_iterator_loc(inner, 0);               \
      const r_ssize x_stride = rray_run_iterator_stride(inner, 0);             \
                                                                               \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const X_CTYPE x_elt = v_x[x_loc];                                      \
        ACCUMULATE;                                                            \
        x_loc += x_stride;                                                     \
      }                                                                        \
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
