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

#endif
