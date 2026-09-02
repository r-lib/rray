static inline void check_axis_names(
  r_obj* names,
  int axis,
  int dimension,
  struct r_lazy error_call
);

static r_obj* rray_broadcast_names_fill(
  r_obj* out,
  r_obj* x,
  r_obj* dimensions
);
