static r_obj* rray_cast_lgl_to_int(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_lgl_to_dbl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_lgl_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_int_to_lgl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_int_to_dbl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_int_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_dbl_to_lgl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_dbl_to_int(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
static r_obj* rray_cast_dbl_to_cpl(
  r_obj* x,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static r_no_return void stop_incompatible_cast(
  enum rray_type x,
  enum rray_type to,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
