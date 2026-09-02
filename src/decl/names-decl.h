static inline void check_axis_names(
  r_obj* names,
  int axis,
  int dimension,
  struct r_lazy error_call
);

static r_obj* rray_names_coalesce(
  r_obj* x_names,
  r_obj* y_names,
  int dimensionality
);
