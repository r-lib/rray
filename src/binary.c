#include "binary.h"

#include "type.h"

r_no_return void stop_unsupported_binary(
  const char* op,
  r_obj* x,
  r_obj* y,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_abort_lazy_call(
    error_call,
    "Can't apply `%s` to %s and %s.",
    op,
    rray_arg_type_format(x_arg, rray_typeof(x)),
    rray_arg_type_format(y_arg, rray_typeof(y))
  );
}
