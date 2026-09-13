#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

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

r_no_return void stop_unsupported_reduce(
  const char* op,
  r_obj* x,
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
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  for (r_ssize i = 0; i < out_size; ++i) {                                     \
    v_out[i] = OUT_INIT;                                                       \
  }                                                                            \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);          \
  const r_ssize run_stride = rray_strided_iterator_plan_run_stride(plan);      \
                                                                               \
  for (struct rray_strided_iterator it = rray_strided_iterator(plan);          \
       !rray_strided_iterator_finished(&it);                                   \
       rray_strided_iterator_next(&it)) {                                      \
    const r_ssize run_start = rray_strided_iterator_run_start(&it);            \
    const r_ssize run_end = run_start + run_size;                              \
    r_ssize loc = rray_strided_iterator_location(&it);                         \
                                                                               \
    if (run_stride == 0) {                                                     \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[loc] = ONE(v_out[loc], v_x[i]);                                  \
      }                                                                        \
    } else {                                                                   \
      for (r_ssize i = run_start; i < run_end; ++i) {                          \
        v_out[loc] = ONE(v_out[loc], v_x[i]);                                  \
        loc += run_stride;                                                     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
