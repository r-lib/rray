wrap <- function(x) {
  .Call(ffi_test_wrap, x)
}
wrapper_data <- function(x) {
  .Call(ffi_test_wrapper_data, x)
}
is_wrapper <- function(x) {
  .Call(ffi_test_is_wrapper, x)
}
is_altrep <- function(x) {
  .Call(ffi_test_is_altrep, x)
}
wrapper_read_access <- function(x) {
  .Call(ffi_test_wrapper_read_access, x)
}
wrapper_pointer_is_stable <- function(x) {
  .Call(ffi_test_wrapper_pointer_is_stable, x)
}
wrapper_set_elt <- function(x) {
  .Call(ffi_test_wrapper_set_elt, x)
}

expect_wraps <- function(w, x) {
  expect_identical(obj_address(wrapper_data(w)), obj_address(x))
}
