#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

#include "decl/iterator-decl.h"

// --------------------------------------------------------------------------

// Core "next" algorithm used by all iterators
//
// Carries a run into axis 1 and up. Axis 0 is always handled by a run cursor
// before this runs, so this always starts at axis 1. Calls `STEP` and `RESET`
// hooks, which are what define each iterator.
#define RRAY_ITERATOR_NEXT(IT, STEP, RESET)                                    \
  for (int i = 1; i < (IT)->point_dimensionality; ++i) {                       \
    ++(IT)->v_point[i];                                                        \
                                                                               \
    if ((IT)->v_point[i] < (IT)->v_point_dimensions[i]) {                      \
      STEP return;                                                             \
    }                                                                          \
                                                                               \
    (IT)->v_point[i] = 0;                                                      \
                                                                               \
    RESET                                                                      \
  }

// --------------------------------------------------------------------------

// Simplest iterator
//
// Walks the multidimensional point space, providing access to the current
// multidimensional point
struct rray_point_iterator {
  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;

  r_ssize size;
  r_ssize index;
};

static inline void rray_point_iterator_init(
  struct rray_point_iterator* it,
  r_ssize size,
  const int* v_point_dimensions,
  int point_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);

  it->size = size;
  it->index = 0;
}

static inline bool rray_point_iterator_finished(
  const struct rray_point_iterator* it
) {
  return it->index == it->size;
}

static inline void rray_point_iterator_next(struct rray_point_iterator* it) {
  it->index += it->v_point_dimensions[0];
  it->v_point[0] = 0;

  RRAY_ITERATOR_NEXT(it, {}, {})
}

struct rray_point_iterator_run {
  int v_point[RRAY_MAX_DIMENSIONALITY];

  r_ssize index;
  r_ssize end;
};

static inline struct rray_point_iterator_run rray_point_iterator_run(
  const struct rray_point_iterator* it
) {
  struct rray_point_iterator_run run;
  memcpy(run.v_point, it->v_point, sizeof(int) * it->point_dimensionality);
  run.index = it->index;
  run.end = it->index + it->v_point_dimensions[0];
  return run;
}

static inline bool rray_point_iterator_run_finished(
  const struct rray_point_iterator_run* run
) {
  return run->index == run->end;
}

static inline const int* rray_point_iterator_run_point(
  const struct rray_point_iterator_run* run
) {
  return run->v_point;
}

static inline r_ssize rray_point_iterator_run_index(
  const struct rray_point_iterator_run* run
) {
  return run->index;
}

static inline void rray_point_iterator_run_next(
  struct rray_point_iterator_run* run
) {
  ++run->v_point[0];
  ++run->index;
}

// --------------------------------------------------------------------------

// Broadcasting iterator
//
// Walks the multidimensional point space. Reports a 1D `location` in an
// alternate subspace.
//
// For broadcasting, the dimensions you broadcast to make up the larger point
// space. This is walked in order. The original dimensions of the array make
// up the subspace. So as you walk the output's point space you can fetch
// `location`s back into your original array to pull from.
//
// For reducing, it's actually a special form of broadcasting. The original
// dimensions of the array are the point space. The reduced dimensions are the
// subspace. So as you walk the original array, you can fetch `location`s into
// the output to accumulate the reduced result at.
struct rray_iterator {
  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;

  r_ssize size;
  r_ssize index;

  r_ssize location;
  r_ssize v_location_strides[RRAY_MAX_DIMENSIONALITY];
};

static inline void rray_iterator_init(
  struct rray_iterator* it,
  r_ssize size,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);

  it->size = size;
  it->index = 0;

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

static inline bool rray_iterator_finished(const struct rray_iterator* it) {
  return it->index == it->size;
}

static inline void rray_iterator_next(struct rray_iterator* it) {
  it->index += it->v_point_dimensions[0];

  RRAY_ITERATOR_NEXT(
    it,
    { it->location += it->v_location_strides[i]; },
    {
      it->location -=
        (it->v_point_dimensions[i] - 1) * it->v_location_strides[i];
    }
  )
}

struct rray_iterator_run {
  r_ssize location;
  r_ssize stride;

  r_ssize index;
  r_ssize end;
};

static inline struct rray_iterator_run rray_iterator_run(
  const struct rray_iterator* it
) {
  return (struct rray_iterator_run) {
    .location = it->location,
    .stride = it->v_location_strides[0],
    .index = it->index,
    .end = it->index + it->v_point_dimensions[0],
  };
}

static inline bool rray_iterator_run_finished(
  const struct rray_iterator_run* run
) {
  return run->index == run->end;
}

static inline r_ssize rray_iterator_run_location(
  const struct rray_iterator_run* run
) {
  return run->location;
}

static inline r_ssize rray_iterator_run_index(
  const struct rray_iterator_run* run
) {
  return run->index;
}

static inline void rray_iterator_run_next(struct rray_iterator_run* run) {
  run->location += run->stride;
  ++run->index;
}

// --------------------------------------------------------------------------

// Same as `rray_iterator`, but reports in two location spaces while only
// walking the point space once
struct rray_iterator2 {
  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;

  r_ssize size;
  r_ssize index;

  r_ssize location1;
  r_ssize v_location1_strides[RRAY_MAX_DIMENSIONALITY];

  r_ssize location2;
  r_ssize v_location2_strides[RRAY_MAX_DIMENSIONALITY];
};

static inline void rray_iterator2_init(
  struct rray_iterator2* it,
  r_ssize size,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location1_dimensions,
  int location1_dimensionality,
  const int* v_location2_dimensions,
  int location2_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);

  it->size = size;
  it->index = 0;

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

static inline bool rray_iterator2_finished(const struct rray_iterator2* it) {
  return it->index == it->size;
}

static inline void rray_iterator2_next(struct rray_iterator2* it) {
  it->index += it->v_point_dimensions[0];

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

// --------------------------------------------------------------------------

struct rray_iterator2_run {
  r_ssize location1;
  r_ssize stride1;

  r_ssize location2;
  r_ssize stride2;

  r_ssize index;
  r_ssize end;
};

static inline struct rray_iterator2_run rray_iterator2_run(
  const struct rray_iterator2* it
) {
  return (struct rray_iterator2_run) {
    .location1 = it->location1,
    .stride1 = it->v_location1_strides[0],
    .location2 = it->location2,
    .stride2 = it->v_location2_strides[0],
    .index = it->index,
    .end = it->index + it->v_point_dimensions[0],
  };
}

static inline bool rray_iterator2_run_finished(
  const struct rray_iterator2_run* run
) {
  return run->index == run->end;
}

static inline r_ssize rray_iterator2_run_index(
  const struct rray_iterator2_run* run
) {
  return run->index;
}

static inline r_ssize rray_iterator2_run_location1(
  const struct rray_iterator2_run* run
) {
  return run->location1;
}

static inline r_ssize rray_iterator2_run_location2(
  const struct rray_iterator2_run* run
) {
  return run->location2;
}

static inline void rray_iterator2_run_next(struct rray_iterator2_run* run) {
  run->location1 += run->stride1;
  run->location2 += run->stride2;
  ++run->index;
}

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

#undef RRAY_ITERATOR_NEXT

#endif
