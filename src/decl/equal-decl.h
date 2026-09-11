typedef r_obj* (*rray_equal_fn)(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);

static r_obj* rray_equality(
  r_obj* x,
  r_obj* y,
  enum rray_equal_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static rray_equal_fn rray_equal_switch(
  r_obj* x,
  r_obj* y,
  enum rray_equal_op op,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
static const char* rray_equal_op_as_c_string(enum rray_equal_op op);

static r_no_return void stop_unsupported_equal(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_equal_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_lgl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_cpl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_int_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_cpl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_dbl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_cpl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);
static r_obj* rray_equal_cpl_cpl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  enum rray_equal_op op
);

static inline int rray_equal_int_one(int x, int y, enum rray_equal_op op);
static inline int rray_equal_dbl_one(
  double x,
  double y,
  enum rray_equal_op op
);
static inline int rray_equal_cpl_one(
  r_complex x,
  r_complex y,
  enum rray_equal_op op
);
