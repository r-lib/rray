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

test_that("from base types", {

  x <- array(1:4, c(1, 2, 2))

  x_int <- vec_cast(x, rray(integer(), dim = c(0, 2, 2)))
  expect_is(x_int, "vctrs_rray")
  expect_equal(dim(x_int), c(1, 2, 2))
  expect_equal(storage.mode(x_int), "integer")

  x_dbl <- vec_cast(x, rray(double(), dim = c(0, 2, 2)))
  expect_equal(storage.mode(x_dbl), "double")

  x_lgl <- vec_cast(matrix(1), rray(logical(), dim = c(0, 1)))
  expect_equal(storage.mode(x_lgl), "logical")

})

test_that("names are lost", {

  nms <- list("foo", "bar")
  x <- rray(1, c(1, 1), dim_names = nms)

  # to another rray - same shape
  res <- vec_cast(x, rray(1, c(1, 1)))
  expect_equal(rray_dim_names(res), rray_empty_dim_names(2))

  # to another rray - different shape
  res <- vec_cast(x, rray(1, c(1, 2)))
  expect_equal(rray_dim_names(res), rray_empty_dim_names(2))

  # to a base type
  res <- vec_cast(x, array(1, c(1, 2)))
  expect_equal(rray_dim_names(res), rray_empty_dim_names(2))

  # from a base type
  x <- array(1, c(1, 1), dimnames = nms)
  res <- vec_cast(x, rray(1, c(1, 1)))
  expect_equal(rray_dim_names(res), rray_empty_dim_names(2))

})

test_that("can broadcast shape", {
  x <- as_rray(array(1, dim = c(1, 2)))

  # Broadcast shape only, not size!
  x_array <- vec_cast(x, array(1, dim = c(2, 2)))
  expect_is(x_array, "matrix")
  expect_equal(dim(x_array), c(1, 2))

  x_array2 <- vec_cast(x, array(1, dim = c(2, 2, 2)))
  expect_is(x_array2, "array")
  expect_equal(dim(x_array2), c(1, 2, 2))

  x_matrix <- vec_cast(x, matrix(1, 2, 2))
  expect_is(x_matrix, "matrix")
  expect_equal(dim(x_matrix), c(1, 2))

  expect_equal(x_array, x_matrix)

  # can broadcast shape with rray's
  y <- as_rray(array(1, dim = c(2, 2, 3)))
  expect_equal(dim(vec_cast(x, y)), c(1, 2, 3))
})

test_that("casting updates inner types", {
  x_int <- rray(1L)

  expect_equal(
    vec_cast(x_int, double()),
    new_array(1)
  )

  expect_equal(
    vec_cast(x_int, matrix(1)),
    matrix(1)
  )

  expect_equal(
    vec_cast(x_int, matrix(FALSE)),
    matrix(TRUE)
  )
})

test_that("using base coercing functions", {
  x <- as_rray(array(1, dim = c(1, 2)))

  x_dbl <- as.double(x)
  expect_is(x_dbl, "numeric")
  expect_equal(dim(x_dbl), NULL)

  x_int <- as.integer(x)
  expect_is(x_int, "integer")
  expect_equal(dim(x_int), NULL)

  x_lgl <- as.logical(x)
  expect_is(x_lgl, "logical")
  expect_equal(dim(x_lgl), NULL)

  # names are not kept
  x_nm <- rray_set_row_names(x, "foo")
  expect_equal(rray_dim_names(as.double(x_nm)), rray_empty_dim_names(1))
})

test_that("from NULL (handled by vctrs)", {
  expect_equal(vec_cast(NULL, rray(1)), NULL)
})

test_that("from unknown types", {
  expect_error(vec_cast("chr", rray(1)), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(rray(1), "chr"), class = "vctrs_error_incompatible_cast")
})
