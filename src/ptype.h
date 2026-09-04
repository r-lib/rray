#ifndef RRAY_PTYPE_H
#define RRAY_PTYPE_H

#include "rlang.h"

#include "arg.h"
#include "type.h"

struct rray_ptypes {
  r_obj* empty_lgl;
  r_obj* empty_int;
  r_obj* empty_dbl;
  r_obj* empty_cpl;
  r_obj* empty_chr;
  r_obj* empty_raw;
  r_obj* empty_list;
};

extern struct rray_ptypes rray_ptypes;

r_obj* rray_ptype2(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

r_obj* rray_ptype_from_type(enum rray_type type);

#endif
