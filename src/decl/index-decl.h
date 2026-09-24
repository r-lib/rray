static r_obj* rray_as_index_arrays(
  r_obj* indices,
  const int* v_dimensions,
  int dimensionality,
  struct rray_arg* indices_arg,
  struct r_lazy error_call
);

static inline r_ssize rray_index_location(
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const r_ssize* v_index_locations,
  r_ssize indices_size
);

static r_obj* rray_index_lgl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_int(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_dbl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_cpl(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_raw(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_chr(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
static r_obj* rray_index_list(
  r_obj* x,
  const r_ssize* v_x_strides,
  const int* const* v_v_index,
  const struct rray_strided_iterator_n_plan* plan
);
