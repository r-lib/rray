#ifndef RRAY_ITERATOR2_H
#define RRAY_ITERATOR2_H

#include "iterator.h"

struct rray_iterator2 {
  int dimensionality;

  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];

  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];

  r_ssize v_location1_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_location2_strides[RRAY_MAX_DIMENSIONALITY];

  r_ssize location1;
  r_ssize location2;
};

static inline void rray_iterator2_init(
  struct rray_iterator2* it,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location1_dimensions,
  int location1_dimensionality,
  const int* v_location2_dimensions,
  int location2_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }

  rray_location_strides_init(
    it->v_location1_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location1_dimensions,
    location1_dimensionality,
    "location1"
  );

  rray_location_strides_init(
    it->v_location2_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location2_dimensions,
    location2_dimensionality,
    "location2"
  );

  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);
  it->location1 = 0;
  it->location2 = 0;
}

static inline r_ssize rray_iterator2_location1(
  const struct rray_iterator2* it
) {
  return it->location1;
}

static inline r_ssize rray_iterator2_location2(
  const struct rray_iterator2* it
) {
  return it->location2;
}

static inline void rray_iterator2_next(struct rray_iterator2* it) {
  for (int i = 0; i < it->dimensionality; ++i) {
    ++it->v_point[i];

    if (it->v_point[i] < it->v_point_dimensions[i]) {
      it->location1 += it->v_location1_strides[i];
      it->location2 += it->v_location2_strides[i];
      return;
    }

    it->v_point[i] = 0;

    it->location1 -=
      (it->v_point_dimensions[i] - 1) * it->v_location1_strides[i];
    it->location2 -=
      (it->v_point_dimensions[i] - 1) * it->v_location2_strides[i];
  }
}

#endif
