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
  check_max_dimensionality(view_dimensionality);

  it->dimensionality = view_dimensionality;

  for (int i = 0; i < view_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_view_dimensions[i];
  }

  r_ssize stride = 1;
  for (int i = 0; i < view_dimensionality; ++i) {
    const int dimension = (i < dimensionality) ? v_dimensions[i] : 1;
    it->v_location_strides[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }

  memset(it->v_point, 0, sizeof(r_ssize) * view_dimensionality);
  it->location = 0;
}

#endif
