#ifndef RRAY_GLOBALS_H
#define RRAY_GLOBALS_H

#include "rlang.h"

struct rray_globals {
  r_obj* one_int;
  r_obj* two_int;
};

extern struct rray_globals rray_globals;

#endif
