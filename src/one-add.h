#ifndef RRAY_ONE_ADD_H
#define RRAY_ONE_ADD_H

#include <limits.h>

#include "rlang.h"

#include "arithmetic.h"

static inline int rray_add_lgl_one(int x, int y, struct r_lazy error_call) {
  if (x == r_globals.na_int) {
    return r_globals.na_int;
  }

  if (y == r_globals.na_lgl) {
    return r_globals.na_int;
  }

  // Since long vectors aren't supported in arrays,
  // we can't ever integer overflow in a logical array

  return x + y;
}

static inline int rray_add_lgl_one_na_rm(
  int x,
  int y,
  struct r_lazy error_call
) {
  if (y == r_globals.na_lgl) {
    return x;
  }

  return x + y;
}

static inline void check_add_int_overflow(
  int x,
  int y,
  struct r_lazy error_call
) {
  if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < -INT_MAX - y)) {
    stop_int_overflow(error_call);
  }
}

static inline int rray_add_int_one(int x, int y, struct r_lazy error_call) {
  if (x == r_globals.na_int || y == r_globals.na_int) {
    return r_globals.na_int;
  }

  check_add_int_overflow(x, y, error_call);

  return x + y;
}

static inline int rray_add_int_one_na_rm(
  int x,
  int y,
  struct r_lazy error_call
) {
  if (y == r_globals.na_int) {
    return x;
  }

  check_add_int_overflow(x, y, error_call);

  return x + y;
}

static inline double rray_add_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
) {
  return x + y;
}

static inline double rray_add_dbl_one_na_rm(
  double x,
  double y,
  struct r_lazy error_call
) {
  if (ISNAN(y)) {
    return x;
  }

  return x + y;
}

static inline r_complex rray_add_cpl_one(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
) {
  return (r_complex) {.r = x.r + y.r, .i = x.i + y.i};
}

static inline r_complex rray_add_cpl_one_na_rm(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
) {
  return (r_complex) {
    .r = rray_add_dbl_one_na_rm(x.r, y.r, error_call),
    .i = rray_add_dbl_one_na_rm(x.i, y.i, error_call),
  };
}

#endif
