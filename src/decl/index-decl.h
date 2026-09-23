static r_obj* rray_as_index_arrays(
  r_obj* indices,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
);

static struct rray_index_plan rray_index_plan(
  r_obj* x_dimensions,
  r_obj* indices,
  r_obj* dimensions,
  struct r_lazy error_call
);

static inline r_ssize rray_index_plan_location(
  const struct rray_index_plan* plan,
  const r_ssize* v_index_locations
);

static inline void rray_index_plan_next(
  const struct rray_index_plan* plan,
  int* v_point,
  r_ssize* v_index_locations
);

static r_obj* rray_index_lgl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_int(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_dbl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_cpl(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_raw(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_chr(
  r_obj* x,
  const struct rray_index_plan* plan
);
static r_obj* rray_index_list(
  r_obj* x,
  const struct rray_index_plan* plan
);
