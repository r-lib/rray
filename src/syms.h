#ifndef RRAY_SYMS_H
#define RRAY_SYMS_H

#include "rlang.h"

struct rray_syms {
  r_obj* to;
  r_obj* x_arg;
  r_obj* y_arg;
  r_obj* to_arg;
  r_obj* dot_arg;
  r_obj* dot_to_arg;
  r_obj* dot_ptype_arg;
  r_obj* call;
  r_obj* dot_call;
};

extern struct rray_syms rray_syms;

#endif
