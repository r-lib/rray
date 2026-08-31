#ifndef RRAY_DIMENSIONALITY_H
#define RRAY_DIMENSIONALITY_H

#include "rlang.h"

#define RRAY_MAX_DIMENSIONALITY 64

int rray_dimensionality(r_obj* x, struct r_lazy error_call);
int rray_dimensionality_from_dimensions(r_obj* dimensions);

void check_max_dimensionality(int dimensionality);

#endif
