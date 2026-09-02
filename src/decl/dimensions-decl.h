static inline void rray_dimensions2(
  int* v_out_dimensions,
  int* p_out_dimensionality,
  r_ssize* v_out_args,
  const int* v_x_dimensions,
  int x_dimensionality,
  r_ssize x_i,
  r_ssize* p_out_i,
  struct rray_arg* p_out_arg,
  struct rray_arg* p_x_arg,
  struct r_lazy error_call
);
