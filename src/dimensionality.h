#ifndef RRAY_DIMENSIONALITY_H
#define RRAY_DIMENSIONALITY_H

#include "rlang.h"

r_ssize rray_dimensionality(r_obj* x, struct r_lazy error_call);
r_ssize rray_dimensionality_from_dimension_sizes(r_obj* dimension_sizes);

#endif
