#ifndef RRAY_INT_128_H
#define RRAY_INT_128_H

#include <limits.h>
#include <stdint.h>

#include "arithmetic.h"

// A signed 128-bit value represented as `hi * 2^64 + lo`.
// `lo` holds the low 64 bits, and `hi` holds the signed high 64 bits.
struct rray_int128 {
  uint64_t lo;
  int64_t hi;
};

// Add an `int` to a signed 128-bit value.
//
// - Cast `y` through `int64_t` to keep its sign, then add to `lo` as an
//   unsigned value. The low part wraps at 2^64.
// - Add -1 to `hi` when `y` is negative, then add 1 if `lo` wrapped.
//
// For example, with `x = {lo = 1, hi = 0}` and `y = -1`, `lo` wraps to 0.
// The upper part becomes `0 - 1 + 1 = 0`, so the sum is 0.
static inline struct rray_int128 rray_int128_add(struct rray_int128 x, int y) {
  const uint64_t lo = x.lo + (uint64_t) (int64_t) y;
  const int64_t hi = x.hi + (y < 0 ? -1 : 0) + (lo < x.lo);
  return (struct rray_int128) {.lo = lo, .hi = hi};
}

// Convert a signed 128-bit value to an R integer or report overflow.
//
// - A nonnegative value fits when `hi` is 0 and `lo` is at most `INT_MAX`.
// - A negative value fits when `hi` is -1 and `lo` is at least
//   `2^64 - INT_MAX`.
// - Negate `lo` as an unsigned value to get its magnitude, cast that to
//   `int`, then negate it again.
// - The lower limit is `-INT_MAX`, since R uses `INT_MIN` for `NA`.
// - Report overflow for values outside those bounds.
//
// For example, `{lo = UINT64_MAX, hi = -1}` represents -1. Negating `lo`
// as an unsigned value gives 1, so the R integer result is -1.
static inline int rray_int128_as_int(
  struct rray_int128 x,
  struct r_lazy error_call
) {
  if (x.hi == 0 && x.lo <= INT_MAX) {
    return (int) x.lo;
  }

  if (x.hi == -1 && x.lo >= -(uint64_t) INT_MAX) {
    return -(int) -x.lo;
  }

  stop_int_overflow(error_call);
}

#endif
