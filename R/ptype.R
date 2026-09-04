rray_ptype <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  check_dots_empty0(...)
  .Call(ffi_rray_ptype, x, environment())
}

rray_ptype2 <- function(
  x,
  y,
  ...,
  x_arg = caller_arg(x),
  y_arg = caller_arg(y),
  call = caller_env()
) {
  check_dots_empty0(...)
  .Call(ffi_rray_ptype2, x, y, environment())
}
