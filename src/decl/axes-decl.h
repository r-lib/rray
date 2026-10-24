static void check_axes_type(
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static void check_has_titles(
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static int axis_from_title(
  r_obj* title,
  r_obj* titles,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static r_obj* arg_as_axes_positions(
  r_obj* axes,
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static r_obj* axes_from_titles(
  r_obj* axes,
  r_obj* titles,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

static void check_axes_unique(
  r_obj* axes,
  r_obj* input,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* axes_sort(r_obj* axes, int dimensionality);
