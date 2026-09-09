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

#define RRAY_POINT_ITERATOR_FOR_EACH(IT, INDEX, V_POINT, ...)                  \
  do {                                                                         \
    struct rray_point_iterator* const iterator = (IT);                         \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    int* V_POINT = iterator->v_point;                                          \
    const int* v_point_dimensions = iterator->v_point_dimensions;              \
    const int point_dimensionality = iterator->point_dimensionality;           \
                                                                               \
    while (INDEX != size) {                                                    \
      __VA_ARGS__                                                              \
      ++INDEX;                                                                 \
                                                                               \
      for (int axis = 0; axis < point_dimensionality; ++axis) {                \
        ++V_POINT[axis];                                                       \
                                                                               \
        if (V_POINT[axis] < v_point_dimensions[axis]) {                        \
          break;                                                               \
        }                                                                      \
                                                                               \
        V_POINT[axis] = 0;                                                     \
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

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
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

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = (r_ssize) v_point_dimensions[i];
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

  it->point_dimensionality = rray__iterator_axes_coalesce(
    it->v_point_dimensions,
    it->v_location_strides,
    point_dimensionality
  );
}

// For-loop-style iteration, giving access to the flat index and mapped
// location.
//
// The loop structure is more complicated than strictly necessary for
// performance reasons. We process a run along the first axis while holding all
// other axes fixed, so the inner loop only advances the index and location by
// fixed increments. Point adjustment happens between runs. This makes the
// inner loop easier for the compiler to optimize and vectorize. In our
// benchmarks, batching rows improved performance by up to 3-4× and helped our
// operations outperform base R in many cases, even with broadcasting support.
#define RRAY_ITERATOR_FOR_EACH(IT, INDEX, LOCATION, ...)                       \
  do {                                                                         \
    /* Unpacking the iterator fields is critical for performance. It gives */  \
    /* the compiler guarantees about loop counters and fixed inputs that */    \
    /* unlock vectorization optimizations. */                                  \
    struct rray_iterator* const iterator = (IT);                               \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_point_dimensions = iterator->v_point_dimensions;          \
    const int point_dimensionality = iterator->point_dimensionality;           \
    r_ssize LOCATION = iterator->location;                                     \
    const r_ssize* v_location_strides = iterator->v_location_strides;          \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize row_stride = v_location_strides[0];                          \
    const r_ssize row_reset = rows * row_stride;                               \
                                                                               \
    while (INDEX != size) {                                                    \
      /* Apply expression for each row */                                      \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION += row_stride;                                                \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION -= row_reset;                                                   \
                                                                               \
      /* We've iterated over the rows of this batch. */                        \
      /* Advance to the start of the next batch of rows. */                    \
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

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
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

  for (int i = 0; i < point_dimensionality; ++i) {
    it->v_point_dimensions[i] = (r_ssize) v_point_dimensions[i];
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

  it->point_dimensionality = rray__iterator_axes_coalesce2(
    it->v_point_dimensions,
    it->v_location1_strides,
    it->v_location2_strides,
    point_dimensionality
  );
}

#define RRAY_ITERATOR2_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)          \
  do {                                                                         \
    struct rray_iterator2* const iterator = (IT);                              \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_point_dimensions = iterator->v_point_dimensions;          \
    const int point_dimensionality = iterator->point_dimensionality;           \
    r_ssize LOCATION1 = iterator->location1;                                   \
    const r_ssize* v_location1_strides = iterator->v_location1_strides;        \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize* v_location2_strides = iterator->v_location2_strides;        \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize row_stride1 = v_location1_strides[0];                        \
    const r_ssize row_stride2 = v_location2_strides[0];                        \
    const r_ssize row_reset1 = rows * row_stride1;                             \
    const r_ssize row_reset2 = rows * row_stride2;                             \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += row_stride1;                                              \
        LOCATION2 += row_stride2;                                              \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_reset1;                                                 \
      LOCATION2 -= row_reset2;                                                 \
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

static inline bool rray__iterator_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
) {
  return left_dimension == 1 || right_dimension == 1 ||
    right_stride == left_dimension * left_stride;
}

static inline int rray__iterator_axes_coalesce(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  int dimensionality
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    const r_ssize right_dimension = v_dimensions[axis];
    const r_ssize left_stride = v_strides[out_axis];
    const r_ssize right_stride = v_strides[axis];

    const bool coalescible = rray__iterator_axes_coalescible(
      left_dimension,
      left_stride,
      right_dimension,
      right_stride
    );

    if (coalescible) {
      if (left_dimension == 1) {
        v_strides[out_axis] = right_stride;
      }
      v_dimensions[out_axis] = left_dimension * right_dimension;
      continue;
    }

    ++out_axis;
    v_dimensions[out_axis] = right_dimension;
    v_strides[out_axis] = right_stride;
  }

  return out_axis + 1;
}

static inline int rray__iterator_axes_coalesce2(
  r_ssize* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    const r_ssize right_dimension = v_dimensions[axis];
    const r_ssize left_stride1 = v_strides1[out_axis];
    const r_ssize right_stride1 = v_strides1[axis];
    const r_ssize left_stride2 = v_strides2[out_axis];
    const r_ssize right_stride2 = v_strides2[axis];

    const bool coalescible1 = rray__iterator_axes_coalescible(
      left_dimension,
      left_stride1,
      right_dimension,
      right_stride1
    );
    const bool coalescible2 = rray__iterator_axes_coalescible(
      left_dimension,
      left_stride2,
      right_dimension,
      right_stride2
    );

    if (coalescible1 && coalescible2) {
      if (left_dimension == 1) {
        v_strides1[out_axis] = right_stride1;
        v_strides2[out_axis] = right_stride2;
      }

      v_dimensions[out_axis] = left_dimension * right_dimension;
      continue;
    }

    ++out_axis;
    v_dimensions[out_axis] = right_dimension;
    v_strides1[out_axis] = right_stride1;
    v_strides2[out_axis] = right_stride2;
  }

  return out_axis + 1;
}

#endif
