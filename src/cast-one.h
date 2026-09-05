#ifndef RRAY_CAST_ONE_H
#define RRAY_CAST_ONE_H

#include "rlang.h"

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

#endif
