#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

// Iterates one step at a time through the `v_point_dimensions` space,
// where each step is recorded in `v_point`
//
// Reports the corresponding 1-D `location` in a second space utilizing
// the same dimensions, but with some axes collapsed to a dimension of 1
struct rray_iterator {
  int dimensionality;

  // Dimensions that bound `v_point`
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];

  // Current multi-dimensional position
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];

  // Column-major strides into the location space.
  // A collapsed axis has a stride of 0, so it contributes nothing.
  r_ssize v_location_strides[RRAY_MAX_DIMENSIONALITY];

  // Current 1-D position derived from `v_location_strides`
  r_ssize location;
};

static inline void rray_location_strides_init(
  r_ssize* v_location_strides,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality,
  const char* location_arg
) {
  if (location_dimensionality > point_dimensionality) {
    r_stop_internal(
      "`%s_dimensionality` of %d can't be greater than "
      "`point_dimensionality` of %d.",
      location_arg,
      location_dimensionality,
      point_dimensionality
    );
  }

  for (int i = 0; i < location_dimensionality; ++i) {
    const int point_dimension = v_point_dimensions[i];
    const int location_dimension = v_location_dimensions[i];

    if (location_dimension != point_dimension && location_dimension != 1) {
      r_stop_internal(
        "Axis %d of `%s` must have a dimension of 1 or %d, not %d.",
        i + 1,
        location_arg,
        point_dimension,
        location_dimension
      );
    }
  }

  r_ssize stride = 1;
  for (int i = 0; i < point_dimensionality; ++i) {
    const int dimension =
      (i < location_dimensionality) ? v_location_dimensions[i] : 1;
    v_location_strides[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }
}

static inline void rray_iterator_init(
  struct rray_iterator* it,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }

  rray_location_strides_init(
    it->v_location_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location_dimensions,
    location_dimensionality,
    "location"
  );

  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);
  it->location = 0;
}

static inline r_ssize rray_iterator_location(const struct rray_iterator* it) {
  return it->location;
}

static inline const r_ssize* rray_iterator_point(
  const struct rray_iterator* it
) {
  return it->v_point;
}

// Advance the iterator by one step, updating `it->location` and
// `it->v_point`
static inline void rray_iterator_next(struct rray_iterator* it) {
  for (int i = 0; i < it->dimensionality; ++i) {
    ++it->v_point[i];

    if (it->v_point[i] < it->v_point_dimensions[i]) {
      it->location += it->v_location_strides[i];
      return;
    }

    it->v_point[i] = 0;

    it->location -= (it->v_point_dimensions[i] - 1) * it->v_location_strides[i];
  }
}

#endif
