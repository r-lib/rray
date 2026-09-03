#ifndef RRAY_BROADCAST_ITERATOR_H
#define RRAY_BROADCAST_ITERATOR_H

#include "iterator.h"

// Initialize a broadcast iterator
//
// - Iterates one step at a time through the "view"s multidimensional space
// - Reports a 1D location mapping back to the original input space
//
// `v_dimensions` can be shorter than `v_view_dimensions`, trailing
// dimensions are padded with 1.
static inline void rray_broadcast_iterator_init(
  struct rray_iterator* it,
  const int* v_dimensions,
  int dimensionality,
  const int* v_view_dimensions,
  int view_dimensionality
) {
  rray_iterator_init(
    it,
    v_view_dimensions,
    view_dimensionality,
    v_dimensions,
    dimensionality
  );
}

#endif
