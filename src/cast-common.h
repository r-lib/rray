#ifndef RRAY_CAST_COMMON_H
#define RRAY_CAST_COMMON_H

#include "rlang.h"

r_obj* rray_cast_common(r_obj* xs, r_obj* to, struct r_lazy error_call);

#endif
