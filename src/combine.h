#ifndef RRAY_COMBINE_H
#define RRAY_COMBINE_H

#include "rlang.h"

r_obj* rray_combine(r_obj* xs, int axis, struct r_lazy error_call);

#endif
