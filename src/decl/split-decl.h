static void check_split_dimensions(
  r_obj* dimensions,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_split_elt_names(
  r_obj* x_names,
  int dimensionality,
  int axis,
  r_obj* x_axis_names,
  int x_axis_start,
  int dimension
);

static void rray_split_fill(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
static void rray_split_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize x_start,
  const struct rray_strided_iterator_plan* plan
);
