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
  rray_iterator_init(
    it,
    v_dimensions,
    dimensionality,
    v_out_dimensions,
    dimensionality
  );
}

#endif
