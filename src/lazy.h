#ifndef RRAY_LAZY_H
#define RRAY_LAZY_H

#include "rlang.h"

// A list that is only allocated on first use
//
// `self` holds the struct memory, and `data_loc` lets `data` reprotect
// itself when it is finally allocated.
struct rray_lazy_list {
  r_obj* self;
  r_obj* data;
  r_keep_loc data_loc;
  r_ssize size;
};

// The result must be protected immediately with `KEEP_RRAY_LAZY_LIST()`
static inline struct rray_lazy_list* new_rray_lazy_list(r_ssize size) {
  r_obj* self = KEEP(r_alloc_raw(sizeof(struct rray_lazy_list)));

  struct rray_lazy_list* p_out = (struct rray_lazy_list*) r_raw_begin(self);
  p_out->self = self;
  p_out->data = r_null;
  p_out->size = size;

  FREE(1);
  return p_out;
}

#define KEEP_RRAY_LAZY_LIST(p_x, p_n)                                          \
  do {                                                                         \
    KEEP((p_x)->self);                                                         \
    KEEP_HERE((p_x)->data, &(p_x)->data_loc);                                  \
    *(p_n) += 2;                                                               \
  } while (0)

static inline r_obj* init_rray_lazy_list(struct rray_lazy_list* p_x) {
  if (p_x->data == r_null) {
    p_x->data = r_alloc_list(p_x->size);
    KEEP_AT(p_x->data, p_x->data_loc);
  }

  return p_x->data;
}

#endif
