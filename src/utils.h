#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call);

#endif
