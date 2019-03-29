context("test-dims")

x_2x3 <- matrix(1, nrow = 2, ncol = 3)
x_1x5 <- matrix(1, nrow = 1, ncol = 5)
x_5 <- rep(1, times = 5)
x_6x1x3 <- array(1, c(6, 1, 3))

test_that("common dims", {
  expect_equal(rray_dims2(vec_dims(x_2x3), vec_dims(x_1x5)), 2L)
  expect_equal(rray_dims2(vec_dims(x_2x3), NULL), 2L)
  expect_equal(rray_dims2(vec_dims(x_5), vec_dims(x_2x3)), 2L)
  expect_equal(rray_dims2(vec_dims(x_6x1x3), vec_dims(x_2x3)), 3L)
})

test_that("corner cases for rray_dims_common()", {
  expect_equal(rray_dims_common(), NULL)
  expect_equal(rray_dims_common(integer()), 1)
  expect_equal(rray_dims_common(integer(), NULL), 1)
  expect_equal(rray_dims_common(NULL), NULL)
})

test_that("common dim", {
  expect_error(rray_dim2(vec_dim(x_2x3), vec_dim(x_1x5)))

  expect_equal(rray_dim2(vec_dim(x_5), vec_dim(x_1x5)), c(5, 5))
  expect_equal(rray_dim2(vec_dim(x_1x5), vec_dim(x_6x1x3)), c(6, 5, 3))
})


