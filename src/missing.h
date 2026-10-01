#ifndef RRAY_MISSING_H
#define RRAY_MISSING_H

#include <stdint.h>
#include <string.h>

#include "rlang.h"

#include "utils.h"

static inline bool rray_lgl_is_missing(int x) {
  return x == r_globals.na_lgl;
}

static inline bool rray_int_is_missing(int x) {
  return x == r_globals.na_int;
}

static inline bool rray_dbl_is_missing(double x) {
  return ISNAN(x);
}

static inline bool rray_dbl_is_na(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  return (bits & 0x7FF00000FFFFFFFF) == 0x7FF00000000007A2;
}

static inline bool rray_cpl_is_missing(r_complex x) {
  // Purposefully bitwise, can help compiler perform vectorization
  return bool_bitwise_or(ISNAN(x.r), ISNAN(x.i));
}

#endif
