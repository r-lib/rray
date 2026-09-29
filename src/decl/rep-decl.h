static r_obj* arg_as_rep_times(
  r_obj* times,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_rep_times_size(
  r_ssize times_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static int rray_rep_dimension(
  int dimension,
  int times,
  struct r_lazy error_call
);

static void rray_rep_fill(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);
static void rray_rep_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_x_dimensions,
  const int* v_out_dimensions,
  int dimensionality,
  int axis,
  r_ssize block_size,
  int times
);

static inline r_ssize rray_rep_x_block(
  const int* v_point,
  const int* v_x_dimensions,
  int dimensionality,
  int axis
);

static r_obj* rray_rep_names(
  r_obj* names,
  const int* v_axes,
  r_ssize axes_size,
  const int* v_times,
  r_ssize times_size
);

static r_obj* rray_rep_axis_names(r_obj* axis_names, int times);
