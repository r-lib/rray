#ifndef RRAY_REDUCE_H
#define RRAY_REDUCE_H

#include "rlang.h"

#include "arg.h"
#include "iterator.h"

typedef r_obj* (*rray_reduce_fn)(
  r_obj* x,
  r_ssize out_size,
  struct rray_iterator* it
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

#define RRAY_REDUCE(OUT_RTYPE, CTYPE, X_CONST_DEREF, OUT_DEREF, ONE)           \
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, out_size));                      \
  CTYPE* v_out = OUT_DEREF(out);                                               \
  r_memset(v_out, 0, sizeof(CTYPE) * out_size);                                \
                                                                               \
  const r_ssize x_size = r_length(x);                                          \
  const CTYPE* v_x = X_CONST_DEREF(x);                                         \
                                                                               \
  for (r_ssize i = 0; i < x_size; ++i) {                                       \
    const r_ssize loc = rray_iterator_location(it);                            \
    v_out[loc] = ONE(v_out[loc], v_x[i]);                                      \
    rray_iterator_next(it);                                                    \
  }                                                                            \
                                                                               \
  FREE(1);                                                                     \
  return out;

#endif
