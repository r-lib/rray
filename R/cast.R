rray_cast <- function(
  x,
  to,
  ...,
  x_arg = caller_arg(x),
  to_arg = "",
  call = caller_env()
) {
  check_dots_empty0(...)
  .Call(ffi_rray_cast, x, to, environment())
}
