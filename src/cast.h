#ifndef RRAY_CAST_H
#define RRAY_CAST_H

#include <limits.h>

#include "rlang.h"

#include "arg.h"
#include "type.h"

r_obj* rray_cast(
  r_obj* x,
  r_obj* to,
  struct rray_arg* x_arg,
  struct rray_arg* to_arg,
  struct r_lazy error_call
);

r_no_return void stop_lossy_cast(
  enum rray_type x,
  enum rray_type to,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

// --------------------------------------------------------------------------
// Lossless

static inline int rray_cast_lgl_to_lgl_one(int x) {
  return x;
}

static inline int rray_cast_int_to_int_one(int x) {
  return x;
}

static inline double rray_cast_dbl_to_dbl_one(double x) {
  return x;
}

static inline r_complex rray_cast_cpl_to_cpl_one(r_complex x) {
  return x;
}

static inline int rray_cast_lgl_to_int_one(int x) {
  return x;
}

static inline double rray_cast_lgl_to_dbl_one(int x) {
  if (x == r_globals.na_lgl) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_lgl_to_cpl_one(int x) {
  const double out = (x == r_globals.na_lgl) ? r_globals.na_dbl : (double) x;
  return (r_complex) {.r = out, .i = 0};
}

static inline double rray_cast_int_to_dbl_one(int x) {
  if (x == r_globals.na_int) {
    return r_globals.na_dbl;
  }

  return (double) x;
}

static inline r_complex rray_cast_int_to_cpl_one(int x) {
  const double out = (x == r_globals.na_int) ? r_globals.na_dbl : (double) x;
  return (r_complex) {.r = out, .i = 0};
}

static inline r_complex rray_cast_dbl_to_cpl_one(double x) {
  return (r_complex) {.r = x, .i = 0};
}

// --------------------------------------------------------------------------
// Lossy

static inline int rray_cast_int_to_lgl_one(
  int x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (x == r_globals.na_int) {
    return r_globals.na_lgl;
  }

  if (x == 0 || x == 1) {
    return x;
  }

  stop_lossy_cast(RRAY_TYPE_integer, RRAY_TYPE_logical, i, x_arg, error_call);
}

static inline int rray_cast_dbl_to_lgl_one(
  double x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (ISNAN(x)) {
    return r_globals.na_lgl;
  }

  if (x == 0) {
    return 0;
  }

  if (x == 1) {
    return 1;
  }

  stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_logical, i, x_arg, error_call);
}

static inline int rray_cast_dbl_to_int_one(
  double x,
  r_ssize i,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  if (ISNAN(x)) {
    return r_globals.na_int;
  }

  if (x <= INT_MIN || x > INT_MAX) {
    stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_integer, i, x_arg, error_call);
  }

  const int out = (int) x;

  if ((double) out != x) {
    stop_lossy_cast(RRAY_TYPE_double, RRAY_TYPE_integer, i, x_arg, error_call);
  }

  return out;
}

#endif
