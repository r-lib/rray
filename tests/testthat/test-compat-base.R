# ------------------------------------------------------------------------------
context("test-xtfrm")

test_that("xtfrm() returns order as a vector", {
  x <- rray(1:2, c(2, 1))
  expect_identical(xtfrm(x), new_matrix(1:2))

  x <- rray(c(1, 2), c(2, 1))
  expect_identical(xtfrm(x), new_matrix(c(1, 2)))
})

test_that("xtfrm() works for 3D", {
  x <- rray(1:6, c(2, 1, 3))
  expect_equal(xtfrm(x), vec_data(x))
})

test_that("xtfrm() for logicals returns integers", {
  x <- rray(c(TRUE, FALSE), c(2, 1))
  expect_equal(xtfrm(x), new_matrix(c(1L, 0L)))
})

# ------------------------------------------------------------------------------
context("test-min")

test_that("vctrs `min()` is type stable", {
  expect_equal(min(rray(TRUE)), rray(TRUE))
})

test_that("`min()` returns a length 1 vector for 1D", {
  expect_equal(min(rray(5:1)), rray(1L))
})

test_that("`min()` returns a length 1 vector for 2D", {
  x <- rray(c(2, 4, 5, 2), c(2, 2))
  expect_equal(
    min(x),
    rray(2)
  )
})

test_that("`min()` returns a length 1 vector for 3D", {
  x <- rray(c(2, 4, 5, 2), c(2, 1, 2))
  expect_equal(
    min(x),
    rray(2)
  )
})

test_that("vctrs `min()` ignores input in `...`", {
  expect_equal(min(rray(2), 1), rray(2))
})

test_that("NAs are removed", {
  expect_equal(
    min(rray(c(NA, 2)), na.rm = TRUE),
    rray(2)
  )
})

# ------------------------------------------------------------------------------
context("test-max")

test_that("vctrs `max()` is type stable", {
  expect_equal(max(rray(TRUE)), rray(TRUE))
})

test_that("`max()` returns a length 1 vector for 1D", {
  expect_equal(max(rray(5:1)), rray(5L))
})

test_that("`max()` returns a length 1 vector for 2D", {
  x <- rray(c(2, 4, 5, 2), c(2, 2))
  expect_equal(
    max(x),
    rray(5)
  )
})

test_that("`max()` returns a length 1 vector for 3D", {
  x <- rray(c(2, 4, 5, 2), c(2, 1, 2))
  expect_equal(
    max(x),
    rray(5)
  )
})

test_that("vctrs `max()` ignores input in `...`", {
  expect_equal(max(rray(2), 1), rray(2))
})

test_that("NAs are removed", {
  expect_equal(
    max(rray(c(NA, 2)), na.rm = TRUE),
    rray(2)
  )
})

# ------------------------------------------------------------------------------
context("test-range")

test_that("vctrs `range()` is type stable", {
  expect_equal(range(rray(TRUE)), rray(c(TRUE, TRUE)))
})

test_that("`range()` returns a length 2 vector for 1D", {
  expect_equal(range(rray(5:1)), rray(c(1L, 5L)))
})

test_that("`range()` returns a length 2 vector for 2D", {
  x <- rray(c(2, 4, 5, 2), c(2, 2))
  expect_equal(
    range(x),
    rray(c(2, 5))
  )
})

test_that("`range()` returns a length 2 vector for 3D", {
  x <- rray(c(2, 4, 5, 2), c(2, 1, 2))
  expect_equal(
    range(x),
    rray(c(2, 5))
  )
})

test_that("vctrs `range()` ignores input in `...`", {
  expect_equal(range(rray(2), 1), rray(c(2, 2)))
})

test_that("NAs are removed", {
  expect_equal(
    range(rray(c(NA, 2)), na.rm = TRUE),
    rray(c(2, 2))
  )
})

# ------------------------------------------------------------------------------
context("test-determinant")

test_that("can compute the determinant using the base R functions", {
  x <- rray(c(2, 3, 4, 1), c(2, 2))
  expect_equal(det(x), -10)
  expect_equal(determinant(x), determinant(as.matrix(x)))
})

# ------------------------------------------------------------------------------
context("test-is-na")

test_that("can check for missing values", {
  x <- rray(c(1, NA, 2), c(3, 1))
  expect_equal(is.na(x), rray(c(FALSE, TRUE, FALSE), c(3, 1)))
})

test_that("dim names are retained", {
  x <- rray(c(1, NA, 2), c(3, 1), dim_names = list(NULL, "c1"))
  expect_equal(
    rray_dim_names(is.na(x)),
    rray_dim_names(x)
  )
})

# ------------------------------------------------------------------------------
context("test-is-na-assignment")

test_that("assigning NA works with entire rows", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  is.na(x) <- c(TRUE, FALSE, TRUE)

  expect_equal(
    x,
    rray(c(NA, 3, NA), c(3, 1, 2))
  )
})

test_that("can use a logical of length 1", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  is.na(x) <- TRUE

  expect_equal(x, rray(NA_real_, c(3, 1, 2)))
})

test_that("logical must recycle", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  expect_error(is.na(x) <- c(TRUE, FALSE))
})

test_that("cannot use positional NA assignment", {
  x <- rray(c(1, 3, 2), c(3, 1, 2))
  expect_error(is.na(x) <- c(1, 2))
})

test_that("dim names are retained when assigning NAs", {
  x <- rray(c(1, 3, 2), c(3, 1), dim_names = list(NULL, "c1"))
  is.na(x) <- c(TRUE, FALSE, TRUE)
  expect_equal(
    rray_dim_names(x),
    list(NULL, "c1")
  )
})

# ------------------------------------------------------------------------------
