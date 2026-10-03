static void check_split_dimensions(
  r_obj* dimensions,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static void rray_split_fill(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);
static void rray_split_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  r_ssize out_end,
  r_ssize x_start,
  r_ssize x_step,
  r_obj* out_elt_dimensions,
  int dimensionality,
  const r_ssize* v_x_strides
);

static r_obj* rray_split_elt_names(
  r_obj* const* v_x_names,
  int dimensionality,
  int axis,
  r_obj* const* v_x_axis_names,
  int x_axis_start,
  int dimension
);
