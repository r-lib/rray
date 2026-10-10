static r_obj* rray_clear_titles(r_obj* x);

static r_obj* rray_clear_axis_title(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static bool rray_titles_have_any(r_obj* titles);
