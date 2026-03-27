#ifndef RRAY_DIMENSIONALITY_H
#define RRAY_DIMENSIONALITY_H

#include "rlang.h"

#define RRAY_MAX_DIMENSIONALITY 64

r_ssize rray_dimensionality(r_obj* x, struct r_lazy error_call);
r_ssize rray_dimensionality_from_dimensions(r_obj* dimensions);

void check_max_dimensionality(r_ssize dimensionality);

#endif
