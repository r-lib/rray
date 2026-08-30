#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

extern r_obj* dimensions_chr;
extern r_obj* dot_dimensions_chr;
extern r_obj* axes_chr;
extern r_obj* axis_chr;

void check_unclassed(r_obj* x, const char* arg, struct r_lazy error_call);

r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call);

r_obj* vec_cast(r_obj* x, r_obj* to, r_obj* x_arg, r_obj* to_arg);

#endif
