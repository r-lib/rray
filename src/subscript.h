#ifndef RRAY_SUBSCRIPT_H
#define RRAY_SUBSCRIPT_H

#include <math.h>

#include "rlang.h"

#include "arg.h"

enum rray_subscript_kind {
  RRAY_SUBSCRIPT_KIND_locations_int,
  RRAY_SUBSCRIPT_KIND_locations_dbl,
  RRAY_SUBSCRIPT_KIND_mask,
  RRAY_SUBSCRIPT_KIND_points_int,
  RRAY_SUBSCRIPT_KIND_points_dbl
};

struct rray_subscript {
  r_obj* index;
  enum rray_subscript_kind kind;
  r_ssize size;
};

struct rray_subscript_summary {
  double min;
  double max;
  r_ssize zeros;
  bool any_missing;
  bool any_fractional;
};

struct rray_subscript rray_as_locations_subscript(
  r_obj* index,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

struct rray_subscript rray_as_mask_subscript(
  r_obj* index,
  r_ssize size,
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

r_ssize rray_mask_size(r_obj* mask, r_ssize size);

struct rray_subscript_summary rray_subscript_summarise(
  r_obj* index,
  r_ssize start,
  r_ssize size
);

r_no_return void stop_subscript_fractional(
  struct rray_arg* index_arg,
  struct r_lazy error_call
);

r_obj* rray_subscript_as_list(struct rray_subscript subscript);

static inline bool rray_location_is_missing_int(int location) {
  return location == r_globals.na_int;
}

static inline bool rray_location_is_missing_dbl(double location) {
  return isnan(location);
}

#endif
