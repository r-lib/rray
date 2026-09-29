static r_obj* arg_as_rep_each_times(
  r_obj* times,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_rep_each_times_size(
  r_ssize times_size,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static int rray_rep_each_dimension(
  int axis_dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
);

static void rray_rep_each_fill(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);
static void rray_rep_each_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_times,
  r_ssize times_size,
  r_ssize block_size,
  int axis_dimension
);

static r_obj* rray_rep_each_names(
  r_obj* names,
  int axis,
  int out_dimension,
  const int* v_times,
  r_ssize times_size
);

static r_obj* rray_rep_each_axis_names(
  r_obj* axis_names,
  int out_dimension,
  const int* v_times,
  r_ssize times_size
);
