static rray_cumulative_fn rray_cumulative_prod_switch(
  r_obj* x,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_cumulative_prod_lgl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_prod_int(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_prod_dbl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
static r_obj* rray_cumulative_prod_cpl(
  r_obj* x,
  r_ssize state_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_state_broadcast_strides,
  struct r_lazy error_call
);
