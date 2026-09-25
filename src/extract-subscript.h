#ifndef RRAY_EXTRACT_SUBSCRIPT_H
#define RRAY_EXTRACT_SUBSCRIPT_H

#include "rlang.h"

#include "arg.h"

enum rray_extract_subscript_kind {
  RRAY_EXTRACT_SUBSCRIPT_KIND_locations_int,
  RRAY_EXTRACT_SUBSCRIPT_KIND_locations_dbl,
  RRAY_EXTRACT_SUBSCRIPT_KIND_mask,
  RRAY_EXTRACT_SUBSCRIPT_KIND_points_int,
  RRAY_EXTRACT_SUBSCRIPT_KIND_points_dbl
};

struct rray_extract_subscript {
  r_obj* index;
  enum rray_extract_subscript_kind kind;
  r_ssize size;
};

// Validates and categorizes `index` as an extract subscript
//
// The goal is to ensure that `index` takes a form we have native C support for.
// This typically does not involve an allocation, but if `index` is a location
// vector containing negative or zero values, then it will allocate to compute
// the complement or drop zeros so the native C loops can be simpler.
struct rray_extract_subscript rray_as_extract_subscript(
  r_obj* index,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

#endif
