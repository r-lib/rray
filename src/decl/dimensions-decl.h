static inline void rray_dimensions2(
  int* v_out_dimensions,
  r_ssize* v_out_args,
  int* p_out_dimensionality,
  r_ssize* p_out_i,
  const int* v_x_dimensions,
  int x_dimensionality,
  r_ssize x_i,
  struct rray_arg* out_arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
