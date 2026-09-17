static r_obj* rray_combine_names(r_obj* xs, r_obj* dimensions, int axis);
static r_obj* rray_combine_axis_names(r_obj* xs, int axis);
static void rray_combine_fill(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_lgl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_int(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_dbl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_cpl(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_raw(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_chr(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
static void rray_combine_fill_list(
  r_obj* x,
  r_obj* out,
  r_ssize out_start,
  const struct rray_strided_iterator2_plan* plan
);
