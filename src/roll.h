#ifndef RRAY_ROLL_H
#define RRAY_ROLL_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_roll(
  r_obj* x,
  r_obj* n,
  r_obj* axes,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

void check_roll_n_not_missing(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
);

// Bound `n` between `[0, dimension]`
static inline int rray_roll_normalize(int n, int dimension) {
  if (dimension == 0) {
    return 0;
  }

  int out = n % dimension;

  if (out < 0) {
    out += dimension;
  }

  return out;
}

#endif
