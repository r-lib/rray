static r_obj* rray_combine_names(
  r_obj* xs,
  r_obj* dimensions,
  int axis,
  int axis_dimension
);
static r_obj* rray_combine_axis_names(r_obj* xs, int axis, int axis_dimension);
static void rray_combine_fill(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
static void rray_combine_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const int* v_x_broadcast_dimensions,
  int out_dimensionality,
  const r_ssize* v_out_strides,
  const r_ssize* v_x_strides
);
