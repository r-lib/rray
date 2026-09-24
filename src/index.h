#ifndef RRAY_INDEX_H
#define RRAY_INDEX_H

#include "rlang.h"

#include "arg.h"

r_obj* rray_index(
  r_obj* x,
  r_obj* indices,
  struct rray_arg* x_arg,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
);

r_obj* rray_as_index_array(
  r_obj* x,
  int dimension,
  bool* p_any_missing,
  struct rray_arg* arg,
  struct r_lazy error_call
);

#endif
