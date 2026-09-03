#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

#include "decl/iterator-decl.h"

// --------------------------------------------------------------------------

// Shared by every iterator's `_next()`. Walks one step through
// `v_point_dimensions`, running `STEP` when an axis advances and
// `RESET` when it wraps back to 0. `i` names the axis in both.
// clang-format off
#define RRAY_ITERATOR_NEXT(IT, STEP, RESET)                                    \
  for (int i = 0; i < (IT)->dimensionality; ++i) {                             \
    ++(IT)->v_point[i];                                                        \
                                                                               \
    if ((IT)->v_point[i] < (IT)->v_point_dimensions[i]) {                      \
      STEP                                                                     \
      return;                                                                  \
    }                                                                          \
                                                                               \
    (IT)->v_point[i] = 0;                                                      \
    RESET                                                                      \
  }
// clang-format on

// --------------------------------------------------------------------------

// Walks the `v_point_dimensions` space one step at a time, recording
// each step in `v_point`. Tracks no location.
struct rray_point_iterator {
  int dimensionality;

  // Dimensions that bound `v_point`
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];

  // Current multi-dimensional position
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

static inline void rray_point_iterator_init(
  struct rray_point_iterator* it,
  const int* v_point_dimensions,
  int point_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);
}

static inline const r_ssize* rray_point_iterator_point(
  const struct rray_point_iterator* it
) {
  return it->v_point;
}

static inline void rray_point_iterator_next(struct rray_point_iterator* it) {
  RRAY_ITERATOR_NEXT(it, {}, {})
}

// --------------------------------------------------------------------------

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
  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);

  rray__location_strides_init(
    it->v_location_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location_dimensions,
    location_dimensionality,
    "location"
  );
  it->location = 0;
}

static inline r_ssize rray_iterator_location(const struct rray_iterator* it) {
  return it->location;
}

static inline void rray_iterator_next(struct rray_iterator* it) {
  RRAY_ITERATOR_NEXT(
    it,
    { it->location += it->v_location_strides[i]; },
    {
      it->location -=
        (it->v_point_dimensions[i] - 1) * it->v_location_strides[i];
    }
  )
}

// --------------------------------------------------------------------------

// Same as `rray_iterator`, but reports in two location spaces while
// only walking the point space once
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
  memset(it->v_point, 0, sizeof(r_ssize) * point_dimensionality);

  rray__location_strides_init(
    it->v_location1_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location1_dimensions,
    location1_dimensionality,
    "location1"
  );
  it->location1 = 0;

  rray__location_strides_init(
    it->v_location2_strides,
    v_point_dimensions,
    point_dimensionality,
    v_location2_dimensions,
    location2_dimensionality,
    "location2"
  );
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
  RRAY_ITERATOR_NEXT(
    it,
    {
      it->location1 += it->v_location1_strides[i];
      it->location2 += it->v_location2_strides[i];
    },
    {
      it->location1 -=
        (it->v_point_dimensions[i] - 1) * it->v_location1_strides[i];
      it->location2 -=
        (it->v_point_dimensions[i] - 1) * it->v_location2_strides[i];
    }
  )
}

#undef RRAY_ITERATOR_NEXT

// --------------------------------------------------------------------------

static inline void rray__location_strides_init(
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

#endif
