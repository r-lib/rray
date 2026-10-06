static rray_reduce2_fn rray_mean_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_mean_lgl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_int(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_dbl(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);

static r_obj* rray_mean_lgl_or_int(
  const int* v_x,
  int na_value,
  bool na_rm,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static void rray_mean_dbl_rescale(
  const double* v_x,
  bool na_rm,
  double* v_out,
  double* v_corrections,
  const r_ssize* v_counts,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static rray_reduce2_fn rray_mean_forced_fallback_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static r_obj* rray_mean_int_forced_fallback(
  r_obj* x,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_mean_int_fallback(
  const int* v_x,
  bool na_rm,
  r_ssize out_size,
  r_ssize count,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides
);

static inline double rray_mean_int64(int64_t sum, r_ssize count);
static inline double rray_mean_int128(struct rray_int128 sum, r_ssize count);
