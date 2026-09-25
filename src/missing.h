#ifndef RRAY_MISSING_H
#define RRAY_MISSING_H

#include "rlang.h"

static inline bool rray_int_is_missing(int x) {
  return x == r_globals.na_int;
}

static inline bool rray_dbl_is_missing(double x) {
  return ISNAN(x);
}

static inline bool rray_cpl_is_missing(r_complex x) {
  // Purposefully bitwise, can help compiler perform vectorization
  return (int) ISNAN(x.r) | (int) ISNAN(x.i);
}

#endif
