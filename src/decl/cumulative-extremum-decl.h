static rray_cumulative_fn rray_cumulative_max_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_cumulative_fn rray_cumulative_min_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_cumulative_max_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_max_int(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_max_dbl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);

static r_obj* rray_cumulative_min_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_min_int(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_min_dbl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
