#ifndef RRAY_SIZE_H
#define RRAY_SIZE_H

#include "rlang.h"

#include "arg.h"

r_ssize rray_size(r_obj* x, struct rray_arg* p_arg, struct r_lazy error_call);

r_ssize rray_size_from_dimensions(const int* v_dimensions, int dimensionality);

#endif
