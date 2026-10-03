#ifndef RRAY_STRIDED_ITERATOR2_H
#define RRAY_STRIDED_ITERATOR2_H

#include "dimensionality.h"
#include "rlang.h"
#include "size.h"

#include "decl/strided-iterator2-decl.h"

// Strided iterator
//
// Optimizes and assists in walking a multidimensional point space defined by
// `v_dimensions`. As it walks, it updates the current `loc`ations in one or
// more alternate subspaces defined by `v_v_strides`.
//
// We've carefully tuned this iterator for both performance and flexibility. The
// caller MUST create the iterator INSIDE the function that utilizes it. If a
// pointer to the iterator is passed around, it can destroy performance due to
// the way the result is compiled.
//
// --------------------------------------------------------------------------
// Examples
//
// For broadcasting, the dimensions you broadcast to make up the larger point
// space. This is walked in order. The original dimensions of the array make
// up the subspace. So as you walk the output's point space you can create
// `location`s back into your original array to pull from.
//
// For reducing, it's actually a special form of broadcasting. The original
// dimensions of the array are the point space. The reduced dimensions are the
// subspace. So as you walk the original array, you can create `location`s into
// the output to accumulate the reduced result at.
//
// For permuting axes, the permuted dimensions make up the point space. The
// original dimensions of the array make up the subspace. So as you walk the
// output's point space you can create `location`s back into your original
// array to pull from.
//
// --------------------------------------------------------------------------
// Optimization - First axis runs
//
// After coalescing, the iterator walks the entire first axis in one inner run
// while holding all later axes fixed. It only updates the later point
// coordinates between runs, rather than checking and carrying them after every
// element. This gives the compiler a small loop where the index and locations
// advance by fixed strides, making it much easier to optimize and vectorize.
// This loop is typically what the caller writes.
//
// Practical examples of when this is useful:
//
// - Identically shaped binary array operations. Adding two [2, 4, 5] arrays
//   coalesces to dimensions [40], so the entire operation is one first axis run
//   where both input locations advance contiguously.
//
// - Broadcasting over a later axis. Broadcasting a [2, 3] array to [2, 3, 4]
//   coalesces to dimensions [6, 4] with location strides [1, 0]. The iterator
//   performs four first axis runs of size 6, copying six contiguous values
//   before updating the later axis.
//
// - Reducing over a later axis. Reducing a [2, 3, 4] array over its third axis
//   also coalesces to dimensions [6, 4] with output strides [1, 0]. Each first
//   axis run accumulates one contiguous slice into six output locations before
//   advancing along the reduced axis.
//
// --------------------------------------------------------------------------
// Optimization - Coalescing
//
// Coalescing axes is an important optimization used to reduce the number of
// axes we have to iterate over by "merging" adjacent compatible ones. This can
// improve performance by 3-5x on its own in some cases.
//
// Practical examples of when this is useful:
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
// Note that all sets of strides must be coalescible, as coalescing changes the
// output dimensionality, so it's all or nothing.
//
// --------------------------------------------------------------------------
// Optimization - Fixed zero stride paths
//
// After coalescing, the first axis is walked by the inner loop. A stride of 0
// on this axis means that a subspace location stays fixed while the point space
// moves along it. Broadcasting uses this to reuse an input value. Reducing uses
// it to accumulate into the same output location. A zero stride on a later axis
// does not use this path because later axes advance between inner runs.
//
// Practical examples of when this is useful:
//
// - Scalar broadcasting across an entire array. Adding a scalar to a [2, 4, 5]
//   array coalesces to point dimensions [40], with array strides [1] and scalar
//   strides [0]. The scalar location stays fixed while the array and output
//   advance contiguously. The same path is used by arithmetic, comparison,
//   equality, and extrema operations.
//
// - Row broadcasting within a matrix. Adding a [1, 4] row to a [2, 4] array
//   gives the array strides [1, 2] and row strides [0, 1]. Each inner run
//   therefore reuses one row value while the array and output advance
//   contiguously.
//
// - Higher dimensional row broadcasting. Adding a [1, 3, 4] array to a
//   [2, 3, 4] array coalesces to dimensions [2, 12], with array strides [1, 2]
//   and broadcast strides [0, 1]. Each of the 12 inner runs uses the fixed
//   path.
//
// - Shared leading dimensions of size 1. Adding [1, 1, 4] to [1, 3, 4]
//   absorbs the shared first axis and produces dimensions [3, 4], with first
//   input strides [0, 1] and second input strides [1, 3].
//
// - Reducing over the first axis. Reducing a [2, 3, 4] array to [1, 3, 4]
//   produces output strides [0, 1, 3], which coalesce to dimensions [2, 12]
//   with output strides [0, 1]. Each inner run accumulates into one fixed
//   output location.
//
// --------------------------------------------------------------------------
// Optimization - Next specialization
//
// The loops that callers write seem to be particularly sensitive to whether or
// not the "next" loop is unrolled on `n`. For the generic "next" function with
// a runtime `n`, it usually isn't unrolled. But most of the time you have
// either one or two inputs, so we manually unroll in next1() and next2(), and
// those should be used where possible.
struct rray_run_iterator {
  r_ssize size;

  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  r_ssize start;
  r_ssize end;
  r_ssize v_loc[RRAY_MAX_INPUTS];

  // Strides for all `n` arrays, laid out axis-major in a flat array as
  // [dimensionality][n], allowing contiguous access when doing "next" calls.
  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY * RRAY_MAX_INPUTS];
  r_ssize n;
};

static inline struct rray_run_iterator rray_run_iterator(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  r_ssize n
) {
  check_dimensionality(dimensionality);

  if (n < 1 || n > RRAY_MAX_INPUTS) {
    r_stop_internal(
      "`n` (%" R_PRI_SSIZE ") must be between 1 and %d.",
      n,
      RRAY_MAX_INPUTS
    );
  }

  struct rray_run_iterator it;

  it.size = rray_size_from_dimensions_checked(
    v_dimensions,
    dimensionality,
    r_lazy_null
  );

  for (int axis = 0; axis < dimensionality; ++axis) {
    it.v_dimensions[axis] = (r_ssize) v_dimensions[axis];
  }

  // Reorder strides to be contiguous during next() calls
  for (int axis = 0; axis < dimensionality; ++axis) {
    r_ssize* v_strides = it.v_strides + axis * n;
    for (r_ssize i = 0; i < n; ++i) {
      v_strides[i] = v_v_strides[i][axis];
    }
  }

  it.dimensionality = rray__run_iterator_axes_coalesce(
    it.v_dimensions,
    it.v_strides,
    n,
    dimensionality
  );

  it.start = 0;
  it.end = it.v_dimensions[0];

  it.n = n;

  for (r_ssize i = 0; i < n; ++i) {
    it.v_loc[i] = 0;
  }

  for (int axis = 0; axis < it.dimensionality; ++axis) {
    it.v_point[axis] = 0;
  }

  return it;
}

static inline struct rray_run_iterator rray_run_iterator1(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides
) {
  const r_ssize* v_v_strides[] = {v_strides};
  return rray_run_iterator(v_dimensions, dimensionality, v_v_strides, 1);
}

static inline struct rray_run_iterator rray_run_iterator2(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides1,
  const r_ssize* v_strides2
) {
  const r_ssize* v_v_strides[] = {v_strides1, v_strides2};
  return rray_run_iterator(v_dimensions, dimensionality, v_v_strides, 2);
}

static inline void rray_run_iterator_reset(struct rray_run_iterator* it) {
  it->start = 0;
  it->end = it->v_dimensions[0];

  for (r_ssize i = 0; i < it->n; ++i) {
    it->v_loc[i] = 0;
  }

  for (int axis = 0; axis < it->dimensionality; ++axis) {
    it->v_point[axis] = 0;
  }
}

static inline r_ssize rray_run_iterator_size(
  const struct rray_run_iterator* it
) {
  return it->size;
}

static inline r_ssize rray_run_iterator_start(
  const struct rray_run_iterator* it
) {
  return it->start;
}

static inline r_ssize rray_run_iterator_end(
  const struct rray_run_iterator* it
) {
  return it->end;
}

static inline r_ssize rray_run_iterator_loc(
  const struct rray_run_iterator* it,
  r_ssize i
) {
  return it->v_loc[i];
}

static inline r_ssize rray_run_iterator_stride(
  const struct rray_run_iterator* it,
  r_ssize i
) {
  return it->v_strides[i];
}

static inline const r_ssize* rray_run_iterator_v_loc(
  const struct rray_run_iterator* it
) {
  return it->v_loc;
}

static inline const r_ssize* rray_run_iterator_v_strides(
  const struct rray_run_iterator* it
) {
  return it->v_strides;
}

static inline bool rray_run_iterator_done(const struct rray_run_iterator* it) {
  return it->start == it->size;
}

static inline void rray_run_iterator_next(struct rray_run_iterator* it) {
  const r_ssize* v_dimensions = it->v_dimensions;
  const int dimensionality = it->dimensionality;
  const r_ssize* v_strides = it->v_strides;
  const r_ssize n = it->n;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize* v_axis_strides = v_strides + axis * n;
    const r_ssize axis_dimension = v_dimensions[axis];

    ++it->v_point[axis];

    if (it->v_point[axis] < axis_dimension) {
      for (r_ssize i = 0; i < n; ++i) {
        it->v_loc[i] += v_axis_strides[i];
      }
      break;
    }

    it->v_point[axis] = 0;

    for (r_ssize i = 0; i < n; ++i) {
      it->v_loc[i] -= (axis_dimension - 1) * v_axis_strides[i];
    }
  }

  it->start = it->end;
  it->end += v_dimensions[0];
}

static inline void rray_run_iterator_next1(struct rray_run_iterator* it) {
  const r_ssize* v_dimensions = it->v_dimensions;
  const int dimensionality = it->dimensionality;
  const r_ssize* v_strides = it->v_strides;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize axis_stride = v_strides[axis];
    const r_ssize axis_dimension = v_dimensions[axis];

    ++it->v_point[axis];

    if (it->v_point[axis] < axis_dimension) {
      it->v_loc[0] += axis_stride;
      break;
    }

    it->v_point[axis] = 0;

    it->v_loc[0] -= (axis_dimension - 1) * axis_stride;
  }

  it->start = it->end;
  it->end += v_dimensions[0];
}

static inline void rray_run_iterator_next2(struct rray_run_iterator* it) {
  const r_ssize* v_dimensions = it->v_dimensions;
  const int dimensionality = it->dimensionality;
  const r_ssize* v_strides = it->v_strides;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize* v_axis_strides = v_strides + axis * 2;
    const r_ssize axis_stride1 = v_axis_strides[0];
    const r_ssize axis_stride2 = v_axis_strides[1];
    const r_ssize axis_dimension = v_dimensions[axis];

    ++it->v_point[axis];

    if (it->v_point[axis] < axis_dimension) {
      it->v_loc[0] += axis_stride1;
      it->v_loc[1] += axis_stride2;
      break;
    }

    it->v_point[axis] = 0;

    it->v_loc[0] -= (axis_dimension - 1) * axis_stride1;
    it->v_loc[1] -= (axis_dimension - 1) * axis_stride2;
  }

  it->start = it->end;
  it->end += v_dimensions[0];
}

static inline void rray_run_iterator_next3(struct rray_run_iterator* it) {
  const r_ssize* v_dimensions = it->v_dimensions;
  const int dimensionality = it->dimensionality;
  const r_ssize* v_strides = it->v_strides;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize* v_axis_strides = v_strides + axis * 3;
    const r_ssize axis_stride1 = v_axis_strides[0];
    const r_ssize axis_stride2 = v_axis_strides[1];
    const r_ssize axis_stride3 = v_axis_strides[2];
    const r_ssize axis_dimension = v_dimensions[axis];

    ++it->v_point[axis];

    if (it->v_point[axis] < axis_dimension) {
      it->v_loc[0] += axis_stride1;
      it->v_loc[1] += axis_stride2;
      it->v_loc[2] += axis_stride3;
      break;
    }

    it->v_point[axis] = 0;

    it->v_loc[0] -= (axis_dimension - 1) * axis_stride1;
    it->v_loc[1] -= (axis_dimension - 1) * axis_stride2;
    it->v_loc[2] -= (axis_dimension - 1) * axis_stride3;
  }

  it->start = it->end;
  it->end += v_dimensions[0];
}

static inline void rray_run_iterator_next4(struct rray_run_iterator* it) {
  const r_ssize* v_dimensions = it->v_dimensions;
  const int dimensionality = it->dimensionality;
  const r_ssize* v_strides = it->v_strides;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize* v_axis_strides = v_strides + axis * 4;
    const r_ssize axis_stride1 = v_axis_strides[0];
    const r_ssize axis_stride2 = v_axis_strides[1];
    const r_ssize axis_stride3 = v_axis_strides[2];
    const r_ssize axis_stride4 = v_axis_strides[3];
    const r_ssize axis_dimension = v_dimensions[axis];

    ++it->v_point[axis];

    if (it->v_point[axis] < axis_dimension) {
      it->v_loc[0] += axis_stride1;
      it->v_loc[1] += axis_stride2;
      it->v_loc[2] += axis_stride3;
      it->v_loc[3] += axis_stride4;
      break;
    }

    it->v_point[axis] = 0;

    it->v_loc[0] -= (axis_dimension - 1) * axis_stride1;
    it->v_loc[1] -= (axis_dimension - 1) * axis_stride2;
    it->v_loc[2] -= (axis_dimension - 1) * axis_stride3;
    it->v_loc[3] -= (axis_dimension - 1) * axis_stride4;
  }

  it->start = it->end;
  it->end += v_dimensions[0];
}

static inline int rray__run_iterator_axes_coalesce(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  r_ssize n,
  int dimensionality
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    r_ssize* v_left_strides = v_strides + out_axis * n;
    const r_ssize right_dimension = v_dimensions[axis];
    const r_ssize* v_right_strides = v_strides + axis * n;

    bool coalescible = true;

    for (r_ssize i = 0; i < n; ++i) {
      if (!rray__run_iterator_axes_coalescible(
            left_dimension,
            v_left_strides[i],
            right_dimension,
            v_right_strides[i]
          )) {
        coalescible = false;
        break;
      }
    }

    if (coalescible) {
      if (left_dimension == 1) {
        for (r_ssize i = 0; i < n; ++i) {
          v_left_strides[i] = v_right_strides[i];
        }
      }
      v_dimensions[out_axis] = left_dimension * right_dimension;
    } else {
      ++out_axis;
      v_dimensions[out_axis] = right_dimension;
      r_ssize* v_out_strides = v_strides + out_axis * n;
      for (r_ssize i = 0; i < n; ++i) {
        v_out_strides[i] = v_right_strides[i];
      }
    }
  }

  return out_axis + 1;
}

static inline bool rray__run_iterator_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
) {
  return left_dimension == 1 || right_dimension == 1 ||
    right_stride == left_dimension * left_stride;
}

#endif
