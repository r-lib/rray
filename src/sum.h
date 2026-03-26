#ifndef RRAY_SUM_H
#define RRAY_SUM_H

#include "rlang.h"

r_obj* rray_sum(r_obj* x, r_obj* axes, bool na_rm, struct r_lazy error_call);

#endif
