#ifndef RRAY_EXTRACT_SUBSCRIPT_H
#define RRAY_EXTRACT_SUBSCRIPT_H

#include <math.h>

#include "rlang.h"

#include "arg.h"
#include "subscript.h"

// Validates and categorizes `index` as an extract subscript
//
// The goal is to ensure that `index` takes a form we have native C support for.
// This typically does not involve an allocation, but if `index` is a location
// vector containing negative or zero values, then it will allocate to compute
// the complement or drop zeros so the native C loops can be simpler.
struct rray_subscript rray_as_extract_subscript(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

// Convert 1 row worth of point coordinates into the corresponding 1D location
static inline r_ssize rray_point_to_location_int(
  const int* v_index,
  r_ssize row,
  r_ssize rows,
  const r_ssize* v_strides,
  int columns
) {
  r_ssize out = 0;

  for (int column = 0; column < columns; ++column) {
    const int coordinate = v_index[row + column * rows];

    if (coordinate == r_globals.na_int) {
      return -1;
    }

    out += (r_ssize) (coordinate - 1) * v_strides[column];
  }

  return out;
}

static inline r_ssize rray_point_to_location_dbl(
  const double* v_index,
  r_ssize row,
  r_ssize rows,
  const r_ssize* v_strides,
  int columns
) {
  r_ssize out = 0;

  for (int column = 0; column < columns; ++column) {
    const double coordinate = v_index[row + column * rows];

    if (isnan(coordinate)) {
      return -1;
    }

    out += ((r_ssize) coordinate - 1) * v_strides[column];
  }

  return out;
}

#endif
