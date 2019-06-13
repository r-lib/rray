context("test-dim-n")

x_5 <- rep(1, times = 5)
x_2x3 <- matrix(1, nrow = 2, ncol = 3)
x_6x1x3 <- array(1, c(6, 1, 3))

test_that("can compute the number of dimensions", {
  expect_equal(rray_dim_n(x_5), 1)
  expect_equal(rray_dim_n(x_2x3), 2)
  expect_equal(rray_dim_n(x_6x1x3), 3)
})

test_that("dimensionality of NULL is 1", {
  expect_equal(rray_dim_n(NULL), 1)
})

test_that("dimensionality of data frames is 1", {
  expect_equal(rray_dim_n(mtcars), 1)
})
