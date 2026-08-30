#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

void check_unclassed(r_obj* x, const char* arg, struct r_lazy error_call);

r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call);

r_obj* vec_cast(r_obj* x, r_obj* to, r_obj* x_arg, r_obj* to_arg);

#endif
