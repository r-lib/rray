#ifndef RRAY_REDUCTION_ITERATOR_H
#define RRAY_REDUCTION_ITERATOR_H

#include "iterator.h"

// Initialize a reduction iterator
//
// - Iterates one step at a time through the input's multidimensional space
// - Reports a 1D location in the `out`'s space
//
// Note that both `v_dimension_sizes` and `v_out_dimension_sizes` have the
// same `dimensionality`, but `v_out_dimension_sizes` has some axes set
// to size of 1.
static inline void rray_reduction_iterator_init(
  struct rray_iterator* it,
  const int* v_dimension_sizes,
  const int* v_out_dimension_sizes,
  r_ssize dimensionality
) {
  check_max_dimensionality(dimensionality);

  it->dimensionality = dimensionality;

  for (r_ssize i = 0; i < dimensionality; ++i) {
    it->v_point_dimension_sizes[i] = v_dimension_sizes[i];
    it->v_location_dimension_sizes[i] = v_out_dimension_sizes[i];
  }

  it->v_location_strides[0] = 1;
  for (r_ssize i = 1; i < dimensionality; ++i) {
    it->v_location_strides[i] =
      it->v_location_strides[i - 1] * it->v_location_dimension_sizes[i - 1];
  }

  memset(it->v_point, 0, sizeof(r_ssize) * dimensionality);
  it->location = 0;
}

#endif
