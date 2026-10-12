#ifndef RRAY_CUMULATIVE_H
#define RRAY_CUMULATIVE_H

#include "rlang.h"

#include "arg.h"
#include "strided-iterator.h"

typedef r_obj* (*rray_cumulative_fn)(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);

typedef rray_cumulative_fn (*rray_cumulative_fn_switch)(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* rray_cumulative(
  r_obj* x,
  int axis,
  rray_cumulative_fn_switch fn_switch,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#define RRAY_CUMULATIVE_ARGS(...) , __VA_ARGS__
#define RRAY_CUMULATIVE_NO_ARGS

#define RRAY_CUMULATIVE(                                                       \
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
    v_state_broadcast_strides                                                  \
  );                                                                           \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, r_length(x)));                   \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  r_obj* state = KEEP(r_alloc_vector(OUT_RTYPE, state_size));                  \
  OUT_CTYPE* v_state = OUT_DEREF(state);                                       \
                                                                               \
  for (r_ssize i = 0; i < state_size; ++i) {                                   \
    v_state[i] = OUT_INIT;                                                     \
  }                                                                            \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {         \
    const r_ssize start = rray_run_iterator_start(&it);                        \
    const r_ssize end = rray_run_iterator_end(&it);                            \
                                                                               \
    r_ssize state_loc = rray_run_iterator_loc(&it, 0);                         \
    const r_ssize state_stride = rray_run_iterator_stride(&it, 0);             \
                                                                               \
    if (state_stride == 0) {                                                   \
      OUT_CTYPE elt = v_state[state_loc];                                      \
                                                                               \
      for (r_ssize i = start; i < end; ++i) {                                  \
        elt = ONE(elt, v_x[i] ONE_ARGS);                                       \
        v_out[i] = elt;                                                        \
      }                                                                        \
                                                                               \
      v_state[state_loc] = elt;                                                \
    } else {                                                                   \
      for (r_ssize i = start; i < end; ++i) {                                  \
        const OUT_CTYPE elt = ONE(v_state[state_loc], v_x[i] ONE_ARGS);        \
        v_state[state_loc] = elt;                                              \
        v_out[i] = elt;                                                        \
        state_loc += state_stride;                                             \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(2);                                                                     \
  return out;

r_no_return void stop_unsupported_cumulative(
  const char* op,
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
