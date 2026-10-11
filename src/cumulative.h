#ifndef RRAY_CUMULATIVE_H
#define RRAY_CUMULATIVE_H

#include "rlang.h"

#include "arg.h"

typedef r_obj* (*rray_cumulative_fn)(
  r_obj* x,
  r_ssize stride,
  r_ssize axis_dimension,
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

#define RRAY_CUMULATIVE(                                                       \
  X_CTYPE,                                                                     \
  X_CONST_DEREF,                                                               \
  OUT_RTYPE,                                                                   \
  OUT_CTYPE,                                                                   \
  OUT_DEREF,                                                                   \
  OUT_INIT,                                                                    \
  ONE                                                                          \
)                                                                              \
  const r_ssize size = r_length(x);                                            \
  const r_ssize block_size = stride * axis_dimension;                          \
                                                                               \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));                          \
  OUT_CTYPE* v_out = OUT_DEREF(out);                                           \
                                                                               \
  const X_CTYPE* v_x = X_CONST_DEREF(x);                                       \
                                                                               \
  if (stride == 1) {                                                           \
    for (r_ssize start = 0; start < size; start += block_size) {               \
      const r_ssize end = start + block_size;                                  \
      OUT_CTYPE elt = OUT_INIT;                                                \
                                                                               \
      for (r_ssize i = start; i < end; ++i) {                                  \
        elt = ONE(elt, v_x[i]);                                                \
        v_out[i] = elt;                                                        \
      }                                                                        \
    }                                                                          \
  } else {                                                                     \
    for (r_ssize start = 0; start < size; start += block_size) {               \
      const r_ssize first_end = start + stride;                                \
      const r_ssize end = start + block_size;                                  \
                                                                               \
      for (r_ssize i = start; i < first_end; ++i) {                            \
        v_out[i] = ONE(OUT_INIT, v_x[i]);                                      \
      }                                                                        \
                                                                               \
      for (r_ssize i = first_end; i < end; ++i) {                              \
        v_out[i] = ONE(v_out[i - stride], v_x[i]);                             \
      }                                                                        \
    }                                                                          \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
