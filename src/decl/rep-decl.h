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
  int dimension,
  bool each,
  const int* v_times,
  struct r_lazy error_call
);

static r_no_return void stop_dimension_too_large(struct r_lazy error_call);

static void rray_rep_copy(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_int(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_raw(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_chr(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
static void rray_rep_copy_list(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);

static r_obj* rray_rep_names(
  r_obj* names,
  int axis,
  r_ssize out_dimension,
  bool each,
  const int* v_times
);

static r_obj* rray_rep_axis_names(
  r_obj* axis_names,
  r_ssize out_dimension,
  bool each,
  const int* v_times
);
