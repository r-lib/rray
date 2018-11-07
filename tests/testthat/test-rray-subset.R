context("test-rray-subset")

test_that("subset doesn't drop dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # 1st col of every dimension
  expect_equal(dim(x[1]), c(3, 1, 2))
  expect_equal(x[1], x[,1])

  # first row
  expect_equal(dim(x[1,]), c(1, 4, 2))

  # first 3rd dim
  expect_equal(dim(x[,,1]), c(3, 4, 1))

  # multiple dimension subset
  expect_equal(dim(x[1,1]), c(1, 1, 2))
  expect_equal(dim(x[1,1,1]), c(1, 1, 1))
})

test_that("subset works on 4D", {
  x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))

  expect_equal(dim(x[1, 1, 1, 1]), c(1, 1, 1, 1))
  expect_equal(dim(x[, , , 1]), c(3, 4, 2, 1))
})

test_that("extract can pull n-dim elements", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  expect_equal(x[[1]], 1)
  expect_equal(x[[2]], 2) # col major

  expect_equal(x[[1, 1, 1]], 1)
  expect_equal(x[[2, 1, 1]], 2)

  # not enough dims
  expect_error(x[[1, 1]])
})

test_that("extract works on 4D", {
  x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))

  expect_equal(x[[1]], 1)
  expect_equal(x[[2]], 2) # col major

  expect_equal(x[[1, 1, 1, 1]], 1)
  expect_equal(x[[2, 1, 1, 1]], 2)
  expect_equal(x[[1, 1, 1, 2]], 25)

  # not enough dims
  expect_error(x[[1, 1]])
})

test_that("0D slicing", {
  x <- new_rray()
  x_dim <- vec_dim(x)

  expect_is(x[0], "vctrs_rray")
  expect_equal(vec_dim(x[0]), x_dim)

  expect_error(x[,0], "incorrect")
  expect_error(x[,,0], "incorrect")

  y <- as_rray(matrix(1:10, ncol = 2))
  y_dim <- vec_dim(y)

  # no columns
  expect_equal(vec_dim(y[0]), c(5L, 0L))
  expect_equal(vec_dim(y[,0]), c(5L, 0L))

  # no rows
  expect_equal(vec_dim(y[0,]), c(0L, 2L))

  expect_error(y[,,0], "incorrect")

  z <- as_rray(array(1, c(1,1,1,1)))
  expect_error(z[,,,,0], "incorrect")
})
