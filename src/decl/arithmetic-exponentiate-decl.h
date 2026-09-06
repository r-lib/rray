static rray_binary_arithmetic_fn rray_exponentiate_switch(
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_obj* rray_exponentiate_lgl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_lgl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_int_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_lgl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_dbl_lgl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_int_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_int_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_dbl_int(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);
static r_obj* rray_exponentiate_dbl_dbl(
  r_obj* x,
  r_obj* y,
  r_ssize size,
  struct rray_iterator2* it,
  struct r_lazy error_call
);

static inline double rray_exponentiate_dbl_one(
  double x,
  double y,
  struct r_lazy error_call
);
