context("test-reducers")

x <- rray(1:10)
y <- rray_reshape(x, c(5, 2))

test_that("Results are correct", {
  expect_equal(vec_data(rray_sum(y, 1)), c(15, 40))
  expect_equal(vec_data(rray_sum(y, 2)), c(7, 9, 11, 13, 15))
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_sum(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_sum(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_sum(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_sum(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_sum(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(6, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  # TODO THESE SHOULD NOT BE ERRORS
  # Eventually https://github.com/QuantStack/xtensor-r/issues/74
  # will be fixed
  expect_error(rray_sum(x, 1))
  expect_error(rray_sum(x_mat, c(1, 2)))

})
