#ifndef RRAY_SLICE_SUBSCRIPT_H
#define RRAY_SLICE_SUBSCRIPT_H

#include "rlang.h"

#include "arg.h"
#include "subscript.h"

struct rray_subscript rray_as_slice_subscript(
  r_obj* index,
  int dimension,
  r_obj* names,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

void check_slice_indices_unnamed(r_obj* indices, struct r_lazy error_call);

void check_slice_indices_size(
  r_obj* indices,
  int dimensionality,
  struct r_lazy error_call
);

r_obj* rray_slice_as_locations(struct rray_subscript subscript);

bool rray_slice_locations_any_missing(
  const int* const* v_v_locations,
  const int* v_dimensions,
  int dimensionality
);

#endif
