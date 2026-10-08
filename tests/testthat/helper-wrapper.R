wrap <- function(x) {
  .Call(ffi_test_wrap, x)
}
wrapper_readonly <- function(x) {
  .Call(ffi_test_wrapper_readonly, x)
}
wrapper_writable <- function(x) {
  .Call(ffi_test_wrapper_writable, x)
}
is_wrapper <- function(x) {
  .Call(ffi_test_is_wrapper, x)
}
wrapper_read_access <- function(x) {
  .Call(ffi_test_wrapper_read_access, x)
}

expect_identical_addresses <- function(object, expected) {
  expect_identical(obj_address(object), obj_address(expected))
}
expect_different_addresses <- function(object, expected) {
  expect_true(obj_address(object) != obj_address(expected))
}
