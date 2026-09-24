#ifndef RRAY_STRIDED_ITERATOR_H
#define RRAY_STRIDED_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"
#include "size.h"
#include "strides.h"

#include "decl/strided-iterator-decl.h"

// Strided iterator plan
//
// Optimizes and assists in walking a multidimensional point space defined by
// `v_dimensions`. As it walks, it updates a user provided `start` in an
// alternate subspace defined by `v_strides`.
//
// For performance and flexibility, the user is responsible for managing the run
// loop along the first axis. We've tried many alternative approaches but they
// tend to tank performance quickly as you increase the level of abstractions.
//
// The plan holds only immutable state. The positions that change as it walks
// are managed by the caller. This lets the compiler keep these positions in
// registers, which has significant performance implications.
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
// For iterator2, note that both sets of location strides must be coalescible,
// as coalescing changes the output dimensionality, so it's all or nothing.
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
// When the inner loop receives the stride as a runtime value, the compiler
// can't prove that the location is fixed and falls back to a scalar loop. The
// public iteration macros check for a zero stride and pass a literal 0 to a
// specialized path. This lets the compiler see that the location does not
// change. For binary operations, it can then hoist the fixed load out of the
// loop and vectorize the remaining stride 1 work.
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
struct rray_strided_iterator_plan {
  r_ssize size;

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_strided_iterator_plan rray_strided_iterator_plan(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides
) {
  check_dimensionality(dimensionality);

  struct rray_strided_iterator_plan plan;

  plan.size = rray_size_from_dimensions_checked(
    v_dimensions,
    dimensionality,
    r_lazy_null
  );

  for (int i = 0; i < dimensionality; ++i) {
    plan.v_dimensions[i] = (r_ssize) v_dimensions[i];
    plan.v_strides[i] = v_strides[i];
  }

  plan.dimensionality = rray__strided_iterator_axes_coalesce(
    plan.v_dimensions,
    plan.v_strides,
    dimensionality
  );

  return plan;
}

// Strided iterator specific to broadcasting
//
// An axis of `from` with a dimension of 1 gets a stride of 0, so it stands
// still while the matching axis of `to` walks. Axes past
// `from_dimensionality` are treated as dimension 1.
static inline struct rray_strided_iterator_plan rray_broadcast_iterator_plan(
  const int* v_from_dimensions,
  int from_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
) {
  check_dimensionality(to_dimensionality);

  rray__check_broadcast_dimensions(
    v_from_dimensions,
    from_dimensionality,
    v_to_dimensions,
    to_dimensionality
  );

  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];

  rray_fill_broadcast_strides_from_dimensions(
    v_from_dimensions,
    from_dimensionality,
    to_dimensionality,
    v_strides
  );

  return rray_strided_iterator_plan(
    v_to_dimensions,
    to_dimensionality,
    v_strides
  );
}

static inline r_ssize rray_strided_iterator_plan_size(
  const struct rray_strided_iterator_plan* plan
) {
  return plan->size;
}
static inline r_ssize rray_strided_iterator_plan_run_size(
  const struct rray_strided_iterator_plan* plan
) {
  return plan->v_dimensions[0];
}
static inline r_ssize rray_strided_iterator_plan_run_stride(
  const struct rray_strided_iterator_plan* plan
) {
  return plan->v_strides[0];
}
static inline void rray_strided_iterator_plan_point_init(
  const struct rray_strided_iterator_plan* plan,
  r_ssize* v_point
) {
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);
}

#define RRAY_STRIDED_ITERATOR_NEXT(START, V_POINT, PLAN)                       \
  for (int axis = 1; axis < PLAN->dimensionality; ++axis) {                    \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < PLAN->v_dimensions[axis]) {                            \
      START += PLAN->v_strides[axis];                                          \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
    START -= (PLAN->v_dimensions[axis] - 1) * PLAN->v_strides[axis];           \
  }

// --------------------------------------------------------------------------

// Same as `rray_strided_iterator_plan`, but reports in two location spaces
// while only walking the point space once
struct rray_strided_iterator2_plan {
  r_ssize size;

  // Since coalescing can multiply two axes' dimensions together, we use an
  // `r_ssize` here even though an individual dimension can't be above an `int`.
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  r_ssize v_strides1[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_strides2[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_strided_iterator2_plan rray_strided_iterator2_plan(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides1,
  const r_ssize* v_strides2
) {
  check_dimensionality(dimensionality);

  struct rray_strided_iterator2_plan plan;

  plan.size = rray_size_from_dimensions_checked(
    v_dimensions,
    dimensionality,
    r_lazy_null
  );

  for (int i = 0; i < dimensionality; ++i) {
    plan.v_dimensions[i] = (r_ssize) v_dimensions[i];
    plan.v_strides1[i] = v_strides1[i];
    plan.v_strides2[i] = v_strides2[i];
  }

  plan.dimensionality = rray__strided_iterator_axes_coalesce2(
    plan.v_dimensions,
    plan.v_strides1,
    plan.v_strides2,
    dimensionality
  );

  return plan;
}

// Same as `rray_broadcast_iterator_plan()`, but broadcasts two `from` spaces
// into one shared `to` space
static inline struct rray_strided_iterator2_plan rray_broadcast_iterator2_plan(
  const int* v_from1_dimensions,
  int from1_dimensionality,
  const int* v_from2_dimensions,
  int from2_dimensionality,
  const int* v_to_dimensions,
  int to_dimensionality
) {
  check_dimensionality(to_dimensionality);

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

  rray_fill_broadcast_strides_from_dimensions(
    v_from1_dimensions,
    from1_dimensionality,
    to_dimensionality,
    v_strides1
  );

  rray_fill_broadcast_strides_from_dimensions(
    v_from2_dimensions,
    from2_dimensionality,
    to_dimensionality,
    v_strides2
  );

  return rray_strided_iterator2_plan(
    v_to_dimensions,
    to_dimensionality,
    v_strides1,
    v_strides2
  );
}

static inline r_ssize rray_strided_iterator2_plan_size(
  const struct rray_strided_iterator2_plan* plan
) {
  return plan->size;
}
static inline r_ssize rray_strided_iterator2_plan_run_size(
  const struct rray_strided_iterator2_plan* plan
) {
  return plan->v_dimensions[0];
}
static inline r_ssize rray_strided_iterator2_plan_run_stride1(
  const struct rray_strided_iterator2_plan* plan
) {
  return plan->v_strides1[0];
}
static inline r_ssize rray_strided_iterator2_plan_run_stride2(
  const struct rray_strided_iterator2_plan* plan
) {
  return plan->v_strides2[0];
}
static inline void rray_strided_iterator2_plan_point_init(
  const struct rray_strided_iterator2_plan* plan,
  r_ssize* v_point
) {
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);
}

#define RRAY_STRIDED_ITERATOR_NEXT2(START1, START2, V_POINT, PLAN)             \
  for (int axis = 1; axis < PLAN->dimensionality; ++axis) {                    \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < PLAN->v_dimensions[axis]) {                            \
      START1 += PLAN->v_strides1[axis];                                        \
      START2 += PLAN->v_strides2[axis];                                        \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
    START1 -= (PLAN->v_dimensions[axis] - 1) * PLAN->v_strides1[axis];         \
    START2 -= (PLAN->v_dimensions[axis] - 1) * PLAN->v_strides2[axis];         \
  }

// --------------------------------------------------------------------------

struct rray_strided_iterator_n_plan {
  r_ssize size;

  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;

  // Strides for all `n` arrays, laid out axis-major as [dimensionality][n].
  const r_ssize* v_strides;
  r_ssize n;
};

static inline struct rray_strided_iterator_n_plan rray_strided_iterator_n_plan(
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides,
  r_ssize n
) {
  check_dimensionality(dimensionality);

  if (n < 1) {
    r_stop_internal("`n` (%" R_PRI_SSIZE ") must be at least 1.", n);
  }

  struct rray_strided_iterator_n_plan plan;

  plan.size = rray_size_from_dimensions_checked(
    v_dimensions,
    dimensionality,
    r_lazy_null
  );

  for (int axis = 0; axis < dimensionality; ++axis) {
    plan.v_dimensions[axis] = (r_ssize) v_dimensions[axis];
  }

  plan.dimensionality = dimensionality;
  plan.v_strides = v_strides;
  plan.n = n;

  return plan;
}

static inline r_ssize rray_strided_iterator_n_plan_size(
  const struct rray_strided_iterator_n_plan* plan
) {
  return plan->size;
}
static inline r_ssize rray_strided_iterator_n_plan_run_size(
  const struct rray_strided_iterator_n_plan* plan
) {
  return plan->v_dimensions[0];
}
static inline r_ssize rray_strided_iterator_n_plan_run_stride(
  const struct rray_strided_iterator_n_plan* plan,
  r_ssize i
) {
  return plan->v_strides[i];
}
static inline void rray_strided_iterator_n_plan_point_init(
  const struct rray_strided_iterator_n_plan* plan,
  r_ssize* v_point
) {
  r_memset(v_point, 0, sizeof(r_ssize) * (size_t) plan->dimensionality);
}

#define RRAY_STRIDED_ITERATOR_NEXTN(V_STARTS, V_POINT, PLAN)                   \
  for (int axis = 0; axis < PLAN->dimensionality; ++axis) {                    \
    const r_ssize* v_axis_strides =                                            \
      PLAN->v_strides + (r_ssize) axis * PLAN->n;                              \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < PLAN->v_dimensions[axis]) {                            \
      for (r_ssize i = 0; i < PLAN->n; ++i) {                                  \
        V_STARTS[i] += v_axis_strides[i];                                      \
      }                                                                        \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
    for (r_ssize i = 0; i < PLAN->n; ++i) {                                    \
      V_STARTS[i] -= (PLAN->v_dimensions[axis] - 1) * v_axis_strides[i];       \
    }                                                                          \
  }

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

#endif
