static rray_reduce_run_fn rray_sum_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_sum_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_cpl(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
static r_obj* rray_sum_cpl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_x_dimensions,
  int dimensionality,
  const r_ssize* const* v_v_strides,
  struct r_lazy error_call
);
