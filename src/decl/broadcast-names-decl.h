static r_obj* rray_broadcast_names_merge(
  r_obj* out,
  r_ssize* v_names_args,
  r_ssize* v_titles_args,
  r_ssize* p_out_i,
  const bool* v_ignored,
  r_obj* x,
  r_ssize x_i,
  r_obj* dimensions,
  struct rray_arg* out_arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static void check_axis_names_equal(
  r_obj* x_names,
  r_obj* y_names,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_no_return void stop_incompatible_names(
  r_obj* x_name,
  r_obj* y_name,
  int axis,
  r_ssize position,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);

static r_no_return void stop_incompatible_titles(
  r_obj* x_title,
  r_obj* y_title,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
);
