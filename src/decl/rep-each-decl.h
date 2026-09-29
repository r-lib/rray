static r_obj* arg_as_rep_each_times(
  r_obj* times,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static r_no_return void stop_rep_each_times_size(
  r_ssize times_size,
  int axis_dimension,
  struct rray_arg* arg,
  struct r_lazy error_call
);

static int rray_rep_each_dimension(
  int axis_dimension,
  const int* v_times,
  r_ssize times_size,
  struct r_lazy error_call
);

static r_obj* rray_rep_each_locations(
  int axis_dimension,
  int out_dimension,
  const int* v_times,
  r_ssize times_size
);
