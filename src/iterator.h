#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

// Iterates one step at a time through the `v_point_dimension_sizes` space,
// where each step is recorded in `v_point`
//
// Reports the corresponding 1-D `location` in `v_location_dimension_sizes`
// space
struct rray_iterator {
  r_ssize dimensionality;

  // Dimension sizes that bound `v_point`
  int v_point_dimension_sizes[RRAY_MAX_DIMENSIONALITY];

  // Current multi-dimensional position
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];

  // Dimension sizes that determine `location`.
  // Size-1 dimensions contribute no stride.
  int v_location_dimension_sizes[RRAY_MAX_DIMENSIONALITY];

  // Column-major strides computed from `v_location_dimension_sizes`
  r_ssize v_location_strides[RRAY_MAX_DIMENSIONALITY];

  // Current 1-D position derived from `v_location_dimension_sizes`
  r_ssize location;
};

static inline r_ssize rray_iterator_location(const struct rray_iterator* it) {
  return it->location;
}

// Advance the iterator by one step, updating `it->location` and
// `it->v_point`
static inline void rray_iterator_next(struct rray_iterator* it) {
  for (r_ssize i = 0; i < it->dimensionality; ++i) {
    ++it->v_point[i];

    if (it->v_point[i] < it->v_point_dimension_sizes[i]) {
      if (it->v_location_dimension_sizes[i] != 1) {
        it->location += it->v_location_strides[i];
      }
      return;
    }

    it->v_point[i] = 0;

    if (it->v_location_dimension_sizes[i] != 1) {
      it->location -=
        (it->v_location_dimension_sizes[i] - 1) * it->v_location_strides[i];
    }
  }
}

#endif
