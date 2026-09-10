typedef r_obj* (*rray_compare_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);

static r_obj* rray_compare(
  r_obj* x,
  r_obj* y,
  enum rray_compare_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static rray_compare_fn rray_compare_switch(
  r_obj* x,
  r_obj* y,
  enum rray_compare_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static const char* rray_compare_op_as_c_string(enum rray_compare_op op);

static r_no_return void stop_unsupported_compare(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_compare_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);
static r_obj* rray_compare_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_compare_op op
);

static inline bool rray_compare_int_is_missing(int x);
static inline bool rray_compare_dbl_is_missing(double x);
