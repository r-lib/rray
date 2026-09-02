#ifndef RRAY_BROADCAST_NAMES_H
#define RRAY_BROADCAST_NAMES_H

#include "rlang.h"

// Broadcasting of array names
//
// Assumes the inputs are validated arrays and are broadcastable to the
// `dimensions`. The caller typically checks all of this already.
r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions);
r_obj* rray_broadcast_names2(r_obj* x, r_obj* y, r_obj* dimensions);
r_obj* rray_broadcast_names_common(r_obj* xs, r_obj* dimensions);

#endif
