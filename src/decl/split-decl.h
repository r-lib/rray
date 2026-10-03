static void check_split_dimensions(
  r_obj* dimensions,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static void rray_split_fill(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);
static void rray_split_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  struct rray_run_iterator* it
);

static r_obj* rray_split_elt_names(
  r_obj* const* v_x_names,
  int dimensionality,
  int axis,
  r_obj* const* v_x_axis_names,
  int x_axis_start,
  int dimension
);
