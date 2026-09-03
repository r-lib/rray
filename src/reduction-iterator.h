#ifndef RRAY_REDUCTION_ITERATOR_H
#define RRAY_REDUCTION_ITERATOR_H

#include "iterator.h"

// Initialize a reduction iterator
//
// - Iterates one step at a time through the input's multidimensional space
// - Reports a 1D location in the `out`'s space
//
// Note that both `v_dimensions` and `v_out_dimensions` have the
// same `dimensionality`, but `v_out_dimensions` has some axes set
// to size of 1.
static inline void rray_reduction_iterator_init(
  struct rray_iterator* it,
  const int* v_dimensions,
  const int* v_out_dimensions,
  int dimensionality
) {
  check_max_dimensionality(dimensionality);

  it->dimensionality = dimensionality;

  for (int i = 0; i < dimensionality; ++i) {
    it->v_point_dimensions[i] = v_dimensions[i];
  }

  r_ssize stride = 1;
  for (int i = 0; i < dimensionality; ++i) {
    const int dimension = v_out_dimensions[i];
    it->v_location_strides[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }

  memset(it->v_point, 0, sizeof(r_ssize) * dimensionality);
  it->location = 0;
}

#endif
