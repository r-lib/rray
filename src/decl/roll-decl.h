static r_obj* arg_as_roll_n(
  r_obj* n,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_roll_n_size(
  r_ssize n_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_obj* rray_roll_locations(int dimension, int n);
