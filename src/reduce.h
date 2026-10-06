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

typedef r_obj* (*rray_reduce2_fn)(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);

typedef rray_reduce2_fn (*rray_reduce2_fn_switch)(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_reduce2(
  r_obj* x,
  r_obj* axes,
  bool na_rm,
  rray_reduce2_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static inline r_ssize rray_reduce_count(r_ssize x_size, r_ssize out_size) {
  return out_size == 0 ? 0 : x_size / out_size;
}

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
// stop_unsupported_reduce

r_no_return void stop_unsupported_reduce(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
