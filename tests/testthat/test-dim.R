context("test-dim")

x_2x3 <- matrix(1, nrow = 2, ncol = 3)
x_1x5 <- matrix(1, nrow = 1, ncol = 5)
x_5 <- rep(1, times = 5)
x_6x1x3 <- array(1, c(6, 1, 3))

test_that("common dim", {
  expect_error(rray_dim2(rray_dim(x_2x3), rray_dim(x_1x5)))

  expect_equal(rray_dim2(rray_dim(x_5), rray_dim(x_1x5)), c(5, 5))
  expect_equal(rray_dim2(rray_dim(x_1x5), rray_dim(x_6x1x3)), c(6, 5, 3))
})

test_that("`dim<-` can expand a 0 dimension if another dimension is 0", {
  x <- rray(numeric(), c(0, 1, 0))
  dim(x) <- c(2, 1, 0)
  expect_equal(rray_dim(x), c(2, 1, 0))
})

test_that("0 size dim handling", {
  x <- c(0, 2)

  # 1 is broadcast down to 0
  expect_equal(rray_dim2(x, c(1, 2)), c(0, 2))

  # 0 is same as 0
  expect_equal(rray_dim2(x, c(0, 2)), c(0, 2))

  # 2 is not equal to 0, and is not 1, so error
  expect_error(rray_dim2(x, c(2, 2)), "\\(0, 2\\) and \\(2, 2\\)")
})

test_that("corner cases for rray_dim_common()", {
  expect_equal(rray_dim_common(), NULL)
  expect_equal(rray_dim_common(integer()), 0)
  expect_equal(rray_dim_common(matrix(logical(), 0, 1)), c(0, 1))
  expect_equal(rray_dim_common(matrix(logical(), 0, 0)), c(0, 0))
  expect_equal(rray_dim_common(integer(), NULL), 0)
  expect_equal(rray_dim_common(NULL), 0)
  expect_equal(rray_dim_common(NULL, matrix()), c(0, 1))
})
