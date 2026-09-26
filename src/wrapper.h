#ifndef RRAY_WRAPPER_H
#define RRAY_WRAPPER_H

#include "rlang.h"

r_obj* r_wrap(r_obj* x);

r_obj* r_clone_data(r_obj* x);

#endif
