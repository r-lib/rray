static inline int rray__run_plan_axes_coalesce(
  r_ssize* v_dimensions,
  r_ssize* v_strides,
  r_ssize n,
  int dimensionality
);

static inline bool rray__run_plan_axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
);
