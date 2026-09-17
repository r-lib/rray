#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include <complex.h>
#include <string.h>

#include "rlang.h"

#include "arg.h"
#include "type.h"

// Operations such as `*`, `/`, and `^` are either more efficient or more
// correct (around infinities) if they go through the C99 `_Complex`. The C99
// standard guarantees that `_Complex` has the same representation as a two
// element array of its real type, so we can `memcpy()` it over. Analysis of
// the assembly shows that this copy disappears entirely, so it's not a
// performance hit.
static inline double _Complex rray_cpl_to_c99(r_complex x) {
  double _Complex out;
  memcpy(&out, &x, sizeof(out));
  return out;
}

static inline r_complex rray_c99_to_cpl(double _Complex x) {
  r_complex out;
  memcpy(&out, &x, sizeof(out));
  return out;
}

void check_unclassed(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_no_return void stop_scalar_input(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* vec_as_array(r_obj* x);

r_obj* arg_as_array(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

r_obj* arg_as_integer(r_obj* x, struct rray_arg* arg);

int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

bool r_has_name_at(r_obj* names, r_ssize i);

int int_add_checked(int x, int y);

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg
);

#endif
