static r_obj* rray_arithmetic(
  enum rray_binary_op op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_add_switch(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);

static r_obj* rray_add_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_add_cpl(
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
