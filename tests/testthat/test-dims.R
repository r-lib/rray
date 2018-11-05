context("test-dims")

x_2x3 <- matrix(1, nrow = 2, ncol = 3)
x_1x5 <- matrix(1, nrow = 1, ncol = 5)
x_5 <- rep(1, times = 5)
x_6x1x3 <- array(1, c(6, 1, 3))

test_that("common dims", {
  expect_equal(mtrx_dims2(x_2x3, x_1x5), 2L)
  expect_equal(mtrx_dims2(x_2x3, NULL), 2L)
  expect_equal(mtrx_dims2(x_5, x_2x3), 2L)
  expect_equal(mtrx_dims2(x_2x2x2, x_2x3), 3L)
})

test_that("common dim", {
  expect_error(mtrx_dim2(x_2x3, x_1x5))

  expect_equal(mtrx_dim2(x_5, x_1x5), c(5, 5))
  expect_equal(mtrx_dim2(x_1x5, x_6x1x3), c(6, 5, 3))
})


