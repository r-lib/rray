context("test-rray-cast")

test_that("to another rray", {
  x <- as_rray(array(1, dim = c(1, 2)))
  y <- as_rray(array(1, dim = c(1, 2)))

  z <- vec_cast(x, y)
  expect_is(z, "vctrs_rray")
  expect_equal(dim(z), c(1, 2))
})

test_that("to base types", {
  x <- as_rray(array(1, dim = c(1, 2)))

  x_array <- vec_cast(x, array(1, dim = c(1, 2)))
  expect_is(x_array, "matrix")
  expect_equal(dim(x_array), c(1, 2))

  x_matrix <- vec_cast(x, matrix(1, 1, 2))
  expect_is(x_matrix, "matrix")
  expect_equal(dim(x_matrix), c(1, 2))

  expect_equal(x_array, x_matrix)

  # can cast down to a integer matrix
  x_int_matrix <- vec_cast(x, matrix(1L, 1, 2))
  expect_equal(storage.mode(x_int_matrix), "integer")
})

test_that("can broadcast shape", {
  x <- as_rray(array(1, dim = c(1, 2)))

  x_array <- vec_cast(x, array(1, dim = c(2, 2)))
  expect_is(x_array, "matrix")
  expect_equal(dim(x_array), c(2, 2))

  x_array2 <- vec_cast(x, array(1, dim = c(2, 2, 2)))
  expect_is(x_array2, "array")
  expect_equal(dim(x_array2), c(2, 2, 2))

  x_matrix <- vec_cast(x, matrix(1, 2, 2))
  expect_is(x_matrix, "matrix")
  expect_equal(dim(x_matrix), c(2, 2))

  expect_equal(x_array, x_matrix)

  # can broadcast shape with rray's
  y <- as_rray(array(1, dim = c(2, 2, 3)))
  expect_equal(dim(vec_cast(x, y)), c(2, 2, 3))
})

test_that("using base coercing functions", {
  x <- as_rray(array(1, dim = c(1, 2)))

  # different behavior from vctrs
  x_dbl <- as.double(x)
  expect_is(x_dbl, "matrix")
  expect_equal(dim(x_dbl), c(1, 2))

  x_int <- as.integer(x)
  expect_is(x_int, "matrix")
  expect_equal(dim(x_int), c(1, 2))
  expect_equal(storage.mode(x_int), "integer")
})
