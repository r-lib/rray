#ifndef RRAY_SLICE_ITERATOR_H
#define RRAY_SLICE_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

static inline r_ssize rray_slice_start(
  const int* const* v_v_locations,
  const r_ssize* v_x_strides,
  const int* v_point,
  int dimensionality,
  bool any_missing
) {
  r_ssize out = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const int* v_locations = v_v_locations[axis];
    const int location =
      v_locations == NULL ? v_point[axis] + 1 : v_locations[v_point[axis]];
    if (any_missing && location == r_globals.na_int) {
      return -1;
    }
    out += ((r_ssize) location - 1) * v_x_strides[axis];
  }

  return out;
}

#define RRAY_SLICE_NEXT_POINT(V_POINT, N)                                      \
  for (int axis = 1; axis < N; ++axis) {                                       \
    ++V_POINT[axis];                                                           \
    if (V_POINT[axis] < v_dimensions[axis]) {                                  \
      break;                                                                   \
    }                                                                          \
    V_POINT[axis] = 0;                                                         \
  }

#define RRAY_SLICE_LOOP(RUN, LOCATION, N, ANY_MISSING, ...)                    \
  while (run_start != size) {                                                  \
    const r_ssize start =                                                      \
      rray_slice_start(v_v_locations, v_x_strides, v_point, N, ANY_MISSING);   \
                                                                               \
    RUN(LOCATION, N, ANY_MISSING, __VA_ARGS__);                                \
                                                                               \
    run_start += run_size;                                                     \
    RRAY_SLICE_NEXT_POINT(v_point, N);                                         \
  }

// Specialization up to 4D allows the compiler to unroll loops in the next
// point computation, which runs around 30% faster in total for those cases
#define RRAY_SLICE_LOOPS(RUN, LOCATION, ...)                                   \
  switch (dimensionality) {                                                    \
  case 1: {                                                                    \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP(RUN, LOCATION, 1, true, __VA_ARGS__);                    \
    } else {                                                                   \
      RRAY_SLICE_LOOP(RUN, LOCATION, 1, false, __VA_ARGS__);                   \
    }                                                                          \
    break;                                                                     \
  }                                                                            \
  case 2: {                                                                    \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP(RUN, LOCATION, 2, true, __VA_ARGS__);                    \
    } else {                                                                   \
      RRAY_SLICE_LOOP(RUN, LOCATION, 2, false, __VA_ARGS__);                   \
    }                                                                          \
    break;                                                                     \
  }                                                                            \
  case 3: {                                                                    \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP(RUN, LOCATION, 3, true, __VA_ARGS__);                    \
    } else {                                                                   \
      RRAY_SLICE_LOOP(RUN, LOCATION, 3, false, __VA_ARGS__);                   \
    }                                                                          \
    break;                                                                     \
  }                                                                            \
  case 4: {                                                                    \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP(RUN, LOCATION, 4, true, __VA_ARGS__);                    \
    } else {                                                                   \
      RRAY_SLICE_LOOP(RUN, LOCATION, 4, false, __VA_ARGS__);                   \
    }                                                                          \
    break;                                                                     \
  }                                                                            \
  default: {                                                                   \
    if (any_missing) {                                                         \
      RRAY_SLICE_LOOP(RUN, LOCATION, dimensionality, true, __VA_ARGS__);       \
    } else {                                                                   \
      RRAY_SLICE_LOOP(RUN, LOCATION, dimensionality, false, __VA_ARGS__);      \
    }                                                                          \
    break;                                                                     \
  }                                                                            \
  }

#define RRAY_SLICE_ITERATE(RUN, ...)                                           \
  const int* v_run_locations = v_v_locations[0];                               \
  const r_ssize run_size = v_dimensions[0];                                    \
  r_ssize run_start = 0;                                                       \
                                                                               \
  int v_point[RRAY_MAX_DIMENSIONALITY] = {0};                                  \
                                                                               \
  if (v_run_locations == NULL) {                                               \
    RRAY_SLICE_LOOPS(RUN, i + 1, __VA_ARGS__);                                 \
  } else {                                                                     \
    RRAY_SLICE_LOOPS(RUN, v_run_locations[i], __VA_ARGS__);                    \
  }

#endif
