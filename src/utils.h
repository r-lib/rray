#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include <complex.h>

#include "rlang.h"

#include "arg.h"
#include "type.h"

// Lets `*`, `/` and `^` match base R, which multiplies through the C99 type
// and so recovers infinities that the hand written formula turns into `NaN`.
// The copy folds away entirely, leaving the plain formula inline and a call to
// `__muldc3` only when both halves come out `NaN`. C99 guarantees a complex
// type has the same representation as a two element array of its real type,
// real part first, which holds on macOS, Windows and Linux without a compiler
// specific path like the C11 `CMPLX()` macro, unavailable on macOS under gcc.
static inline double _Complex rray_cpl_to_c99(r_complex x) {
  double _Complex out;
  r_memcpy(&out, &x, sizeof(out));
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

int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);

bool r_has_name_at(r_obj* names, r_ssize i);

r_obj* vec_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg
);

#endif
