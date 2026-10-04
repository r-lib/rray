static rray_reduce_fn rray_mean_switch(
  r_obj* x,
  bool na_rm,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_mean_lgl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_lgl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_int(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_int_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_dbl(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_dbl_na_rm(
  r_obj* x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);

static r_obj* rray_mean_lgl_or_int(
  const int* v_x,
  int na_value,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);
static r_obj* rray_mean_lgl_or_int_na_rm(
  const int* v_x,
  int na_value,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);
static void rray_mean_lgl_or_int_propagate_na(
  const int* v_x,
  int na_value,
  double* v_out,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static r_obj* rray_mean_int_fallback(
  const int* v_x,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);
static r_obj* rray_mean_int_na_rm_fallback(
  const int* v_x,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static void rray_mean_dbl_rescale(
  const double* v_x,
  double* v_out,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);
static void rray_mean_dbl_rescale_na_rm(
  const double* v_x,
  double* v_out,
  const r_ssize* v_counts,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);
static void rray_mean_dbl_propagate_na(
  const double* v_x,
  double* v_out,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static inline r_ssize rray_mean_count(r_ssize x_size, r_ssize out_size);
static inline double rray_mean_int64(int64_t sum, r_ssize count);
