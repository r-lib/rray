#ifndef RRAY_STRIDED_ITERATOR_H
#define RRAY_STRIDED_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"
#include "size.h"

#include "decl/strided-iterator-decl.h"

// Strided iterator
//
// Walks the multidimensional point space defined by `v_dimensions`. Reports a
// 1D `location` in an alternate subspace defined by `v_strides`.
//
// --------------------------------------------------------------------------
// Examples
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
// to pull from.
//
// --------------------------------------------------------------------------
// Optimization - Coalescing
//
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
//
// For iterator2, note that both sets of location strides must be coalescible,
// as coalescing changes the output dimensionality, so it's all or nothing.
//
// --------------------------------------------------------------------------
// Optimization - Fixed zero stride paths
//
// A stride of 0 means that a subspace location stays fixed while the point
// space moves along that axis. Broadcasting uses this to reuse an input value.
// Reducing uses it to accumulate into the same output location. When the inner
// loop receives the stride as a runtime value, the compiler can't prove that
// the location is fixed and falls back to a scalar loop. The public iteration
// macros check for a zero stride and pass a literal 0 to a specialized path.
// This lets the compiler see that the location does not change. For binary
// operations, it can then hoist the fixed load out of the loop and vectorize
// the remaining stride 1 work.
//
// - Scalar broadcasting across an entire array. Adding a scalar to a [2, 4, 5]
//   array coalesces to one dimension [40] with stride pair [1, 0]. The scalar
//   location stays fixed while the array and output advance contiguously. The
//   same path is used by arithmetic, comparison, equality, and extrema
//   operations.
//
// - Row broadcasting within a matrix. Adding a [1, 4] row to a [2, 4] array
//   gives the array strides [1, 2] and row strides [0, 1]. Each inner run has
//   stride pair [1, 0], so it reuses one row value while the array and output
//   advance contiguously.
//
// - Higher dimensional row broadcasting. Adding a [1, 3, 4] array to a
//   [2, 3, 4] array coalesces to dimensions [2, 12], with array strides [1, 2]
//   and broadcast strides [0, 1]. Each of the 12 inner runs uses the fixed
//   path.
//
// - Shared leading dimensions of size 1. Adding [1, 1, 4] to [1, 3, 4]
//   absorbs the shared first axis and produces dimensions [3, 4], with a stride
//   pair [0, 1] along the first coalesced axis.
//
// - Reducing over the first axis. Reducing a [2, 3, 4] array to [1, 3, 4]
//   produces output strides [0, 1, 3], which coalesce to dimensions [2, 12]
//   with output strides [0, 1]. Each inner run accumulates into one fixed
//   output location.
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

// Strided iterator specific to broadcasting
//
// An axis of `from` with a dimension of 1 gets a stride of 0, so it stands
// still while the matching axis of `to` walks. Axes past
// `from_dimensionality` are treated as dimension 1.
static inline struct rray_strided_iterator rray_broadcast_iterator(
  const int* v_from_dimensions,
  int from_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
) {
  check_max_dimensionality(to_dimensionality);

  rray__check_broadcast_dimensions(
    v_from_dimensions,
    from_dimensionality,
    v_to_dimensions,
    to_dimensionality
  );

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];

  rray__fill_broadcast_strides(
    v_from_dimensions,
    from_dimensionality,
    to_dimensionality,
    v_strides
  );

  return rray_strided_iterator(v_to_dimensions, to_dimensionality, v_strides);
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
    struct rray_strided_iterator* const iterator = (IT);                       \
    const r_ssize* v_strides = iterator->v_strides;                            \
                                                                               \
    if (v_strides[0] == 0) {                                                   \
      RRAY__STRIDED_ITERATOR_FOR_EACH(IT, INDEX, LOCATION, 0, __VA_ARGS__);    \
    } else {                                                                   \
      RRAY__STRIDED_ITERATOR_FOR_EACH(                                         \
        IT,                                                                    \
        INDEX,                                                                 \
        LOCATION,                                                              \
        v_strides[0],                                                          \
        __VA_ARGS__                                                            \
      );                                                                       \
    }                                                                          \
  } while (0)

#define RRAY__STRIDED_ITERATOR_FOR_EACH(IT, INDEX, LOCATION, ROW_STRIDE, ...)  \
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
    const r_ssize row_reset = rows * ROW_STRIDE;                               \
                                                                               \
    while (INDEX != size) {                                                    \
      /* Apply expression for each row */                                      \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION += ROW_STRIDE;                                                \
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
        LOCATION -= (v_dimensions[axis] - 1) * v_strides[axis];                \
      }                                                                        \
    }                                                                          \
  } while (0)

// --------------------------------------------------------------------------

// Same as `rray_strided_iterator`, but reports in two location spaces while
// only walking the point space once
struct rray_strided_iterator2 {
  r_ssize index;
  r_ssize size;

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  r_ssize location1;
  r_ssize v_strides1[RRAY_MAX_DIMENSIONALITY];

  r_ssize location2;
  r_ssize v_strides2[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_strided_iterator2 rray_strided_iterator2(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides1,
  const r_ssize* v_strides2
) {
  check_max_dimensionality(dimensionality);

  struct rray_strided_iterator2 it;

  it.index = 0;
  it.size = rray_size_from_dimensions(v_dimensions, dimensionality);

  for (int i = 0; i < dimensionality; ++i) {
    it.v_dimensions[i] = (r_ssize) v_dimensions[i];
    it.v_strides1[i] = v_strides1[i];
    it.v_strides2[i] = v_strides2[i];
  }
  memset(it.v_point, 0, sizeof(r_ssize) * dimensionality);
  it.location1 = 0;
  it.location2 = 0;

  it.dimensionality = rray__strided_iterator_axes_coalesce2(
    it.v_dimensions,
    it.v_strides1,
    it.v_strides2,
    dimensionality
  );

  return it;
}

// Same as `rray_broadcast_iterator()`, but broadcasts two `from` spaces into
// one shared `to` space
static inline struct rray_strided_iterator2 rray_broadcast_iterator2(
  const int* v_from1_dimensions,
  int from1_dimensionality,
  const int* v_from2_dimensions,
  int from2_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
) {
  check_max_dimensionality(to_dimensionality);

  rray__check_broadcast_dimensions(
    v_from1_dimensions,
    from1_dimensionality,
    v_to_dimensions,
    to_dimensionality
  );

  rray__check_broadcast_dimensions(
    v_from2_dimensions,
    from2_dimensionality,
    v_to_dimensions,
    to_dimensionality
  );

  r_ssize v_strides1[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_strides2[RRAY_MAX_DIMENSIONALITY];

  rray__fill_broadcast_strides(
    v_from1_dimensions,
    from1_dimensionality,
    to_dimensionality,
    v_strides1
  );

  rray__fill_broadcast_strides(
    v_from2_dimensions,
    from2_dimensionality,
    to_dimensionality,
    v_strides2
  );

  return rray_strided_iterator2(
    v_to_dimensions,
    to_dimensionality,
    v_strides1,
    v_strides2
  );
}

#define RRAY_STRIDED_ITERATOR2_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)  \
  do {                                                                         \
    struct rray_strided_iterator2* const iterator = (IT);                      \
    const r_ssize* v_strides1 = iterator->v_strides1;                          \
    const r_ssize* v_strides2 = iterator->v_strides2;                          \
                                                                               \
    if (v_strides1[0] == 0) {                                                  \
      RRAY__STRIDED_ITERATOR2_FOR_EACH(                                        \
        IT,                                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        0,                                                                     \
        v_strides2[0],                                                         \
        __VA_ARGS__                                                            \
      );                                                                       \
    } else if (v_strides2[0] == 0) {                                           \
      RRAY__STRIDED_ITERATOR2_FOR_EACH(                                        \
        IT,                                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        v_strides1[0],                                                         \
        0,                                                                     \
        __VA_ARGS__                                                            \
      );                                                                       \
    } else {                                                                   \
      RRAY__STRIDED_ITERATOR2_FOR_EACH(                                        \
        IT,                                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        v_strides1[0],                                                         \
        v_strides2[0],                                                         \
        __VA_ARGS__                                                            \
      );                                                                       \
    }                                                                          \
  } while (0)

#define RRAY__STRIDED_ITERATOR2_FOR_EACH(                                      \
  IT,                                                                          \
  INDEX,                                                                       \
  LOCATION1,                                                                   \
  LOCATION2,                                                                   \
  ROW_STRIDE1,                                                                 \
  ROW_STRIDE2,                                                                 \
  ...                                                                          \
)                                                                              \
  do {                                                                         \
    struct rray_strided_iterator2* const iterator = (IT);                      \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_dimensions = iterator->v_dimensions;                      \
    const int dimensionality = iterator->dimensionality;                       \
    r_ssize LOCATION1 = iterator->location1;                                   \
    const r_ssize* v_strides1 = iterator->v_strides1;                          \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize* v_strides2 = iterator->v_strides2;                          \
                                                                               \
    const r_ssize rows = v_dimensions[0];                                      \
    const r_ssize row_reset1 = rows * ROW_STRIDE1;                             \
    const r_ssize row_reset2 = rows * ROW_STRIDE2;                             \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += ROW_STRIDE1;                                              \
        LOCATION2 += ROW_STRIDE2;                                              \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_reset1;                                                 \
      LOCATION2 -= row_reset2;                                                 \
                                                                               \
      for (int axis = 1; axis < dimensionality; ++axis) {                      \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_dimensions[axis]) {                              \
          LOCATION1 += v_strides1[axis];                                       \
          LOCATION2 += v_strides2[axis];                                       \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION1 -= (v_dimensions[axis] - 1) * v_strides1[axis];              \
        LOCATION2 -= (v_dimensions[axis] - 1) * v_strides2[axis];              \
      }                                                                        \
    }                                                                          \
  } while (0)

// --------------------------------------------------------------------------

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

static inline int rray__strided_iterator_axes_coalesce2(
  r_ssize* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    const r_ssize left_stride1 = v_strides1[out_axis];
    const r_ssize left_stride2 = v_strides2[out_axis];
    const r_ssize right_dimension = v_dimensions[axis];
    const r_ssize right_stride1 = v_strides1[axis];
    const r_ssize right_stride2 = v_strides2[axis];

    const bool coalescible1 = rray__strided_iterator_axes_coalescible(
      left_dimension,
      left_stride1,
      right_dimension,
      right_stride1
    );
    const bool coalescible2 = rray__strided_iterator_axes_coalescible(
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
    } else {
      ++out_axis;
      v_dimensions[out_axis] = right_dimension;
      v_strides1[out_axis] = right_stride1;
      v_strides2[out_axis] = right_stride2;
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

static inline void rray__check_broadcast_dimensions(
  const int* v_from_dimensions,
  int from_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
) {
  if (from_dimensionality > to_dimensionality) {
    r_stop_internal(
      "Can't broadcast from dimensionality %d to %d. "
      "Can't decrease dimensionality.",
      from_dimensionality,
      to_dimensionality
    );
  }

  for (int i = 0; i < from_dimensionality; ++i) {
    const int from_dimension = v_from_dimensions[i];
    const int to_dimension = v_to_dimensions[i];

    if (from_dimension != to_dimension && from_dimension != 1) {
      r_stop_internal(
        "Can't broadcast axis %d from dimension %d to %d.",
        i + 1,
        from_dimension,
        to_dimension
      );
    }
  }
}

static inline void rray__fill_broadcast_strides(
  const int* v_from_dimensions,
  int from_dimensionality,
  int to_dimensionality,
  r_ssize* v_out
) {
  r_ssize stride = 1;

  for (int i = 0; i < to_dimensionality; ++i) {
    const int dimension = (i < from_dimensionality) ? v_from_dimensions[i] : 1;
    v_out[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }
}

#endif
