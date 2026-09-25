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
  r_obj* i;
  enum rray_extract_subscript_kind kind;
  r_ssize size;
};

struct rray_extract_subscript rray_as_extract_subscript(
  r_obj* i,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* i_arg,
  struct r_lazy error_call
);

#endif
