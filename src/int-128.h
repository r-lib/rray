#ifndef RRAY_INT_128_H
#define RRAY_INT_128_H

#include <limits.h>
#include <stdint.h>

#include "arithmetic.h"

struct rray_int128 {
  uint64_t lo;
  int64_t hi;
};

static inline struct rray_int128 rray_int128_add(
  struct rray_int128 sum,
  int x
) {
  const uint64_t lo = sum.lo + (uint64_t) (int64_t) x;
  const int64_t hi = sum.hi + (x < 0 ? -1 : 0) + (lo < sum.lo);
  return (struct rray_int128) {.lo = lo, .hi = hi};
}

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
