static r_ssize rray_combine_size(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
static r_obj* rray_combine_names(r_obj* xs, r_obj* dimensions, int axis);
static r_obj* rray_combine_axis_names(r_obj* xs, int axis);
static void rray_combine_copy(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
