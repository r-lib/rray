static r_ssize fill_arg_buffer(
  struct rray_arg* arg,
  char* buf,
  r_ssize cur_size,
  r_ssize tot_size
);

static r_ssize str_arg_fill(const char* data, char* buf, r_ssize remaining);

static r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining);

static r_ssize lazy_arg_fill(void* data, char* buf, r_ssize remaining);

static r_ssize subscript_arg_fill(void* data, char* buf, r_ssize remaining);

static r_ssize column_arg_fill(void* data, char* buf, r_ssize remaining);
