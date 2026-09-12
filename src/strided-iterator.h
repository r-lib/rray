#ifndef RRAY_STRIDED_ITERATOR_H
#define RRAY_STRIDED_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"
#include "size.h"

#include "decl/strided-iterator-decl.h"

// Strided iterator
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
//
// For permuting axes, the permuted dimensions make up the point space. The
// original dimensions of the array make up the subspace. So as you walk the
// output's point space you can fetch `location`s back into your original array
// to pull from, except the strides are permuted rather than derived from the
// subspace dimensions.
struct rray_strided_iterator {
  r_ssize index;
  r_ssize size;

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  r_ssize location;
  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_strided_iterator rray_strided_iterator(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides
) {
  check_max_dimensionality(dimensionality);

  struct rray_strided_iterator it;

  it.index = 0;
  it.size = rray_size_from_dimensions(v_dimensions, dimensionality);

  for (int i = 0; i < dimensionality; ++i) {
    it.v_dimensions[i] = (r_ssize) v_dimensions[i];
    it.v_strides[i] = v_strides[i];
  }
  memset(it.v_point, 0, sizeof(r_ssize) * dimensionality);
  it.location = 0;

  it.dimensionality = rray__strided_iterator_axes_coalesce(
    it.v_dimensions,
    it.v_strides,
    dimensionality
  );

  return it;
}

static inline r_ssize rray_strided_iterator_size(
  const struct rray_strided_iterator* it
) {
  return it->size;
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
#define RRAY_STRIDED_ITERATOR_FOR_EACH(IT, INDEX, LOCATION, ...)               \
  do {                                                                         \
    /* Unpacking the iterator fields is critical for performance. It gives */  \
    /* the compiler guarantees about loop counters and fixed inputs that */    \
    /* unlock vectorization optimizations. */                                  \
    struct rray_strided_iterator* const iterator = (IT);                       \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_dimensions = iterator->v_dimensions;                      \
    const int dimensionality = iterator->dimensionality;                       \
    r_ssize LOCATION = iterator->location;                                     \
    const r_ssize* v_strides = iterator->v_strides;                            \
                                                                               \
    const r_ssize rows = v_dimensions[0];                                      \
    const r_ssize row_stride = v_strides[0];                                   \
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
      for (int axis = 1; axis < dimensionality; ++axis) {                      \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_dimensions[axis]) {                              \
          LOCATION += v_strides[axis];                                         \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION -= (v_dimensions[axis] - 1) * v_strides[axis];                \
      }                                                                        \
    }                                                                          \
  } while (0)

// Coalescing axes is an important optimization used to reduce the number of
// axes we have to iterate over by "merging" adjacent compatible ones. This can
// 3-5x performance on its own in some cases. It's easiest to look at some
// practical applications of how this is useful:
//
// - Identically shaped binary array operations. Adding two arrays with
//   dimensions [2, 4, 5] gives both inputs strides [1, 2, 8]. All adjacent axes
//   coalesce (2 * 1 = 2, 4 * 2 = 8) into one dimension [40] with stride [1],
//   resulting in one flat vectorizable loop.
//
// - Scalar broadcasting across an entire array. Adding a scalar to a [2, 4, 5]
//   array gives the array strides [1, 2, 8] and the scalar strides [0, 0, 0].
//   Both coalesce completely, producing a [40] loop with array stride [1] and
//   scalar stride [0].
//
// - Contiguous adjacent axes within a broadcast operation. Broadcasting a
//   [2, 3] array over point dimensions [2, 3, 4] produces location strides
//   [1, 2, 0]. The adjacent axes 1 and 2 coalesce (2 * 1 = 2) into size 6,
//   leaving dimensions [6, 4] and strides [1, 0].
//
// - Contiguous regions within a reduction. Reducing a [2, 3, 4] array over its
//   third axis maps into a [2, 3] result using location strides [1, 2, 0].
//   Coalescing produces dimensions [6, 4], making it the same as the broadcast
//   example above.
//
// - Point-space dimensions of size 1. Output point dimensions of [1, 3, 4] can
//   have strides [0, 1, 3]. The dimension 1 first axis is absorbed into the
//   dimension 3 second axis, adopting stride 1, after which the dimension 4
//   third axis also coalesces. The result is one dimension [12] with stride
//   [1]. Note that this isn't broadcasting. This is when both input and output
//   have an axis that stays dimension 1, which is somewhat rare.
static inline int rray__strided_iterator_axes_coalesce(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  int dimensionality
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    const r_ssize left_stride = v_strides[out_axis];
    const r_ssize right_dimension = v_dimensions[axis];
    const r_ssize right_stride = v_strides[axis];

    const bool coalescible = rray__strided_iterator_axes_coalescible(
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
    } else {
      ++out_axis;
      v_dimensions[out_axis] = right_dimension;
      v_strides[out_axis] = right_stride;
    }
  }

  return out_axis + 1;
}

static inline bool rray__strided_iterator_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
) {
  return left_dimension == 1 || right_dimension == 1 ||
    right_stride == left_dimension * left_stride;
}

#endif
