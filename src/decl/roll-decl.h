static r_obj* arg_as_roll_n(
  r_obj* n,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_roll_n_size(
  r_ssize n_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_roll_locations(int dimension, int n);

static r_obj* arg_as_roll_each_n(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_roll_each_normalize(
  r_obj* n,
  const struct rray_strided_iterator_plan* plan,
  int axis_dimension
);

static void rray_roll_each_fill(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_lgl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_int(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_dbl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_cpl(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_raw(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_chr(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);
static void rray_roll_each_fill_list(
  r_obj* x,
  r_obj* out,
  const int* v_n,
  r_ssize block_size,
  int axis_dimension,
  r_ssize n_groups
);

static void check_roll_n_not_missing(
  r_obj* n,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static inline int rray_roll_normalize(int n, int dimension);
