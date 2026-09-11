#ifndef RRAY_GATHER_H
#define RRAY_GATHER_H

#include "rlang.h"

#include "iterator.h"

r_obj* rray_gather(r_obj* x, r_ssize size, struct rray_iterator* it);

#endif
