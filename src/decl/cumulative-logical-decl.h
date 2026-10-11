static rray_cumulative_fn rray_cumulative_all_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);
static rray_cumulative_fn rray_cumulative_any_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_cumulative_all_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_any_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
