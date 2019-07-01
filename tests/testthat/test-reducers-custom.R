# context("test-reducers-custom")
#
# x <- rray(1:4)
# y <- rray_reshape(x, c(2, 2))
#
# test_that("Results are equivalent to rray_sum()", {
#   # sum converts integers to doubles
#   expect_equal(rray_sum(y, axes = 1), rray_reduce_dbl(y, sum))
#   expect_equal(rray_sum(y, axes = 2), rray_reduce_dbl(y, sum, axes = 2))
# })
#
# test_that("Lambda functions are allowed", {
#   expect_equal(
#     vec_data(rray_reduce_dbl(y, ~.x / .y)),
#     c(.5, .75)
#   )
# })
#
# test_that("Dimension names are kept", {
#
#   yy <- rray_set_col_names(y, c("c1", "c2"))
#   yy <- rray_set_row_names(yy, letters[1:2])
#
#   expect_equal(
#     rray_col_names(rray_reduce_int(yy, ~.x + .y)),
#     c("c1", "c2")
#   )
#
#   expect_equal(
#     rray_row_names(rray_reduce_int(yy, ~.x + .y, axes = 2)),
#     letters[1:2]
#   )
#
# })
#
# test_that("Can reduce over multiple axes", {
#   out <- rray_reduce_dbl(rray(1, c(2, 3, 4)), ~ .x + .y, axes = c(1, 2))
#   expect_equal(vec_data(out), rep(6, times = 4))
#   expect_equal(rray_dim(out), c(1, 1, 4))
# })
#
# test_that("Can reduce base R objects", {
#   y_mat <- as.matrix(y)
#   expect_equal(
#     rray_reduce_int(y_mat, ~.x + .y),
#     matrix(c(3L, 7L), ncol = 2, dimnames = list(NULL, NULL))
#   )
# })
#
# test_that("Reducing where x is initially int then becomes dbl works", {
#   x <- rray(1:5, c(5, 1))
#
#   # informative to see what is happening
#   # if its working, we get 1, 1.5, 2, 2.5
#   # if not working, get 1 1 1 1
#   # rray_reduce_dbl(x, ~{print(.x); .x + .5})
#
#   # If this doesn't work, the result is 1.5 because .x is coerced
#   # down to int each time. Bad!
#   expect_equal(
#     vec_data(rray_reduce_dbl(x, ~.x + .5)),
#     3
#   )
# })
#
# test_that("Reducing to 0D works", {
#
#   x_mat <- as.matrix(x)
#
#   sum_1D <- rray_reduce_dbl(x, `+`, axes = 1L)
#   sum_2D <- rray_reduce_dbl(x_mat, `+`, axes = c(1, 2))
#
#   expect_equal(rray_dim(sum_1D), 1L)
#   expect_equal(rray_dim(sum_2D), c(1L, 1L))
#
#   expect_equal(vec_data(sum_1D), 10)
#   expect_equal(vec_data(sum_2D), 10)
# })
