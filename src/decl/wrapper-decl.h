#ifndef RRAY_WRAPPER_DECL_H
#define RRAY_WRAPPER_DECL_H

#include <R_ext/Altrep.h>

#include "rlang.h"

static inline bool is_wrapper(r_obj* x);
static inline R_altrep_class_t wrapper_class(enum r_type type);
static inline r_obj* wrapper_readonly(r_obj* x);

static inline r_obj* r_clone_data(r_obj* x);

#endif
