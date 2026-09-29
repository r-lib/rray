static r_obj* arg_as_rep_times(
  r_obj* times,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_rep_times_size(
  r_ssize times_size,
  r_ssize axes_size,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static int rray_rep_dimension(
  int dimension,
  int times,
  struct r_lazy error_call
);

static r_obj* rray_rep_locations(int dimension, int out_dimension, int times);
