#ifndef RRAY_RESHAPE_H
#define RRAY_RESHAPE_H

#include "rlang.h"

r_obj* rray_reshape(r_obj* x, r_obj* dimension_sizes, struct r_lazy error_call);

#endif
