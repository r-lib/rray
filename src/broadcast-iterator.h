#ifndef RRAY_BROADCAST_ITERATOR_H
#define RRAY_BROADCAST_ITERATOR_H

#include <string.h>

#include "rlang.h"

#define RRAY_MAX_DIMENSIONALITY 64

struct rray_broadcast_iterator {
  R_xlen_t dimensionality;

  int dimension_sizes[RRAY_MAX_DIMENSIONALITY];
  int view_dimension_sizes[RRAY_MAX_DIMENSIONALITY];
  R_xlen_t strides[RRAY_MAX_DIMENSIONALITY];

  // Current position in multi-dimensional space
  R_xlen_t point[RRAY_MAX_DIMENSIONALITY];

  // Current position in 1-D space
  R_xlen_t location;
};

// Initialize the iterator. `v_dimension_sizes` can be shorter than
// `v_view_dimension_sizes` — trailing dimensions are padded with 1.
static inline void rray_broadcast_iterator_init(
  struct rray_broadcast_iterator* it,
  const int* v_dimension_sizes,
  R_xlen_t dimensionality,
  const int* v_view_dimension_sizes,
  R_xlen_t view_dimensionality
) {
  it->dimensionality = view_dimensionality;

  for (R_xlen_t i = 0; i < dimensionality; ++i) {
    it->dimension_sizes[i] = v_dimension_sizes[i];
  }
  for (R_xlen_t i = dimensionality; i < view_dimensionality; ++i) {
    it->dimension_sizes[i] = 1;
  }

  for (R_xlen_t i = 0; i < view_dimensionality; ++i) {
    it->view_dimension_sizes[i] = v_view_dimension_sizes[i];
  }

  it->strides[0] = 1;
  for (R_xlen_t i = 1; i < view_dimensionality; ++i) {
    it->strides[i] = it->strides[i - 1] * it->dimension_sizes[i - 1];
  }

  memset(it->point, 0, sizeof(R_xlen_t) * view_dimensionality);
  it->location = 0;
}

static inline R_xlen_t rray_broadcast_iterator_location(
  const struct rray_broadcast_iterator* it
) {
  return it->location;
}

// Advance the iterator by one step in column-major order.
static inline void rray_broadcast_iterator_next(
  struct rray_broadcast_iterator* it
) {
  for (R_xlen_t i = 0; i < it->dimensionality; ++i) {
    ++it->point[i];

    if (it->point[i] < it->view_dimension_sizes[i]) {
      if (it->dimension_sizes[i] != 1) {
        it->location += it->strides[i];
      }
      return;
    }

    it->point[i] = 0;

    if (it->dimension_sizes[i] != 1) {
      it->location -= (it->dimension_sizes[i] - 1) * it->strides[i];
    }
  }
}

#endif
