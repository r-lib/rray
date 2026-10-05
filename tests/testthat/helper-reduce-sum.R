rray_sum_forced_fallback <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_test_rray_sum_forced_fallback, x, axes, na_rm, environment())
}
