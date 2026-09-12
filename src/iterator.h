#ifndef RRAY_ITERATOR_H
#define RRAY_ITERATOR_H

#include "dimensionality.h"
#include "rlang.h"

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

#endif
