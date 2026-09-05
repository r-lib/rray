static rray_binary_arithmetic_core_fn rray_add_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_add_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_lgl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_int_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);

static inline int rray_add_int_one(int x, int y, struct r_lazy error_call);
static inline double rray_add_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
);
static inline r_complex rray_add_cpl_one(
  r_complex x,
  r_complex y,
  struct r_lazy error_call
);

static r_no_return void stop_int_overflow(struct r_lazy error_call);
