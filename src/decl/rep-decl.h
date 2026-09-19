static r_obj* rray_rep_impl(
  r_obj* x,
  r_obj* times,
  int axis,
  bool each,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* arg_as_times(
  r_obj* times,
  r_ssize size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_times_size(
  r_ssize times_size,
  r_ssize size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static int rray_rep_dimension(
  int axis_dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
);

static r_no_return void stop_dimension_too_large(struct r_lazy error_call);

static void rray_rep_fill_uniform(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_int(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_raw(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_chr(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);
static void rray_rep_fill_uniform_list(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks,
  int times
);

static void rray_rep_fill_varying(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_int(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_raw(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_chr(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);
static void rray_rep_fill_varying_list(
  r_obj* x,
  r_obj* out,
  r_ssize block_size,
  r_ssize n_blocks_per_group,
  r_ssize n_groups,
  const int* v_times
);

static r_obj* rray_rep_names(
  r_obj* names,
  int axis,
  r_ssize out_dimension,
  bool each,
  const int* v_times,
  r_ssize times_size
);

static r_obj* rray_rep_axis_names(
  r_obj* axis_names,
  r_ssize out_dimension,
  bool each,
  const int* v_times,
  r_ssize times_size
);
