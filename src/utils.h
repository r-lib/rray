#ifndef RRAY_UTILS_H
#define RRAY_UTILS_H

#include "rlang.h"

#define RRAY_ARG_SIZE 32

extern r_obj* dimensions_chr;
extern r_obj* dot_dimensions_chr;
extern r_obj* axes_chr;
extern r_obj* axis_chr;

void check_unclassed(r_obj* x, const char* arg, struct r_lazy error_call);

r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call);

int arg_as_int(r_obj* x, r_obj* arg, struct r_lazy error_call);

const char* arg_from_xs(r_obj* xs_names, r_ssize i, char buffer[RRAY_ARG_SIZE]);

r_obj* vec_cast(r_obj* x, r_obj* to, r_obj* x_arg, r_obj* to_arg);

#endif
