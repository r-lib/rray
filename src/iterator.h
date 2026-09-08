#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

#include "decl/iterator-decl.h"

// --------------------------------------------------------------------------

// Simplest iterator
//
// Walks the multidimensional point space, providing access to the current
// multidimensional point
struct rray_point_iterator {
  r_ssize index;
  r_ssize size;

  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;
};

static inline void rray_point_iterator_init(
  struct rray_point_iterator* it,
  r_ssize size,
  const int* v_point_dimensions,
  int point_dimensionality
) {
  check_max_dimensionality(point_dimensionality);

  it->index = 0;
  it->size = size;

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);
}

#define RRAY_POINT_ITERATOR_FOR_EACH(IT, INDEX, POINT, ...)                    \
  do {                                                                         \
    const struct rray_point_iterator* iterator = (IT);                         \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    const int point_dimensionality = iterator->point_dimensionality;           \
    const int* v_point_dimensions = iterator->v_point_dimensions;              \
    int POINT[RRAY_MAX_DIMENSIONALITY];                                        \
    memcpy(POINT, iterator->v_point, sizeof(int) * point_dimensionality);      \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        ++POINT[0];                                                            \
        ++INDEX;                                                               \
      }                                                                        \
      POINT[0] = 0;                                                            \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++POINT[axis];                                                         \
                                                                               \
        if (POINT[axis] < v_point_dimensions[axis]) {                          \
          break;                                                               \
        }                                                                      \
                                                                               \
        POINT[axis] = 0;                                                       \
      }                                                                        \
    }                                                                          \
  } while (0)

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
  r_ssize index;
  r_ssize size;

  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;

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

  it->index = 0;
  it->size = size;

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);

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

#define RRAY_ITERATOR_FOR_EACH(IT, INDEX, LOCATION, ...)                       \
  do {                                                                         \
    const struct rray_iterator* iterator = (IT);                               \
    r_ssize INDEX = iterator->index;                                           \
    r_ssize LOCATION = iterator->location;                                     \
    const r_ssize size = iterator->size;                                       \
    const int point_dimensionality = iterator->point_dimensionality;           \
    const int* v_point_dimensions = iterator->v_point_dimensions;              \
    const r_ssize* v_location_strides = iterator->v_location_strides;          \
    int v_point[RRAY_MAX_DIMENSIONALITY];                                      \
    memcpy(v_point, iterator->v_point, sizeof(int) * point_dimensionality);    \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize stride = v_location_strides[0];                              \
    const r_ssize row_rewind = rows * stride;                                  \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION += stride;                                                    \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION -= row_rewind;                                                  \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION += v_location_strides[axis];                                \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION -= (v_point_dimensions[axis] - 1) * v_location_strides[axis]; \
      }                                                                        \
    }                                                                          \
  } while (0)

// --------------------------------------------------------------------------

// Same as `rray_iterator`, but reports in two location spaces while only
// walking the point space once
struct rray_iterator2 {
  r_ssize index;
  r_ssize size;

  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;

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

  it->index = 0;
  it->size = size;

  it->point_dimensionality = point_dimensionality;

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = v_point_dimensions[i];
  }
  memset(it->v_point, 0, sizeof(int) * point_dimensionality);

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

// --------------------------------------------------------------------------

#define RRAY_ITERATOR2_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)          \
  do {                                                                         \
    const struct rray_iterator2* iterator = (IT);                              \
    r_ssize INDEX = iterator->index;                                           \
    r_ssize LOCATION1 = iterator->location1;                                   \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize size = iterator->size;                                       \
    const int point_dimensionality = iterator->point_dimensionality;           \
    const int* v_point_dimensions = iterator->v_point_dimensions;              \
    const r_ssize* v_location1_strides = iterator->v_location1_strides;        \
    const r_ssize* v_location2_strides = iterator->v_location2_strides;        \
    int v_point[RRAY_MAX_DIMENSIONALITY];                                      \
    memcpy(v_point, iterator->v_point, sizeof(int) * point_dimensionality);    \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize stride1 = v_location1_strides[0];                            \
    const r_ssize stride2 = v_location2_strides[0];                            \
    const r_ssize row_rewind1 = rows * stride1;                                \
    const r_ssize row_rewind2 = rows * stride2;                                \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += stride1;                                                  \
        LOCATION2 += stride2;                                                  \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_rewind1;                                                \
      LOCATION2 -= row_rewind2;                                                \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION1 += v_location1_strides[axis];                              \
          LOCATION2 += v_location2_strides[axis];                              \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION1 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location1_strides[axis];          \
        LOCATION2 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location2_strides[axis];          \
      }                                                                        \
    }                                                                          \
  } while (0)

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
