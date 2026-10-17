view <- function(x) {
  .Call(ffi_test_view, x)
}
view_data <- function(x) {
  .Call(ffi_test_view_data, x)
}
is_view <- function(x) {
  .Call(ffi_test_is_view, x)
}
is_altrep <- function(x) {
  .Call(ffi_test_is_altrep, x)
}
view_read_access <- function(x) {
  .Call(ffi_test_view_read_access, x)
}
view_pointer_is_stable <- function(x) {
  .Call(ffi_test_view_pointer_is_stable, x)
}
view_set_elt <- function(x) {
  .Call(ffi_test_view_set_elt, x)
}

expect_view_of <- function(v, x) {
  expect_identical(obj_address(view_data(v)), obj_address(x))
}
