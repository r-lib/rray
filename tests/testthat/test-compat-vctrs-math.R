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

test_that("`min()` returns a length 1 vector for 1D", {
  expect_equal(min(rray(5:1)), 1L)
  expect_equal(min(rray(5:1 + 0)), 1)
})

test_that("`min()` returns a length 1 vector for 2D", {
  x <- rray(c(2, 4, 5, 2), c(2, 2))
  expect_equal(
    min(x),
    2
  )
})

test_that("`min()` returns a length 1 vector for 3D", {
  x <- rray(c(2, 4, 5, 2), c(2, 1, 2))
  expect_equal(
    min(x),
    2
  )
})

test_that("vctrs `min()` ignores input in `...`", {
  expect_equal(min(rray(2), 1), 2)
})

test_that("NAs are removed", {
  expect_equal(
    min(rray(c(NA, 2)), na.rm = TRUE),
    2
  )
})

# ------------------------------------------------------------------------------
context("test-max")

test_that("`max()` returns a length 1 vector for 1D", {
  expect_equal(max(rray(5:1)), 5L)
  expect_equal(max(rray(5:1 + 0)), 5)
})

test_that("`max()` returns a length 1 vector for 2D", {
  x <- rray(c(2, 4, 5, 2), c(2, 2))
  expect_equal(
    max(x),
    5
  )
})

test_that("`max()` returns a length 1 vector for 3D", {
  x <- rray(c(2, 4, 5, 2), c(2, 1, 2))
  expect_equal(
    max(x),
    5
  )
})

test_that("vctrs `max()` ignores input in `...`", {
  expect_equal(max(rray(2), 1), 2)
})

test_that("NAs are removed", {
  expect_equal(
    max(rray(c(NA, 2)), na.rm = TRUE),
    2
  )
})

# ------------------------------------------------------------------------------
context("test-base-any")

test_that("returns a single value with shaped arrays", {
  expect_equal(any(rray(c(TRUE, FALSE), c(2, 2))), TRUE)
  expect_equal(any(rray(c(FALSE, FALSE), c(2, 2))), FALSE)
})

test_that("always uses `na.rm = TRUE`", {
  expect_equal(any(rray(c(NA, 1L)), na.rm = FALSE), TRUE)
  expect_equal(any(rray(c(NA, 0L)), na.rm = FALSE), FALSE)
})

# ------------------------------------------------------------------------------
context("test-base-all")

test_that("returns a single value with shaped arrays", {
  expect_equal(all(rray(c(TRUE, FALSE), c(2, 2))), FALSE)
  expect_equal(all(rray(c(TRUE, TRUE), c(2, 2))), TRUE)
})

test_that("always uses `na.rm = TRUE`", {
  expect_equal(all(rray(c(NA, 1L, 0L)), na.rm = FALSE), FALSE)
  expect_equal(all(rray(c(NA, 1L, 1L)), na.rm = FALSE), TRUE)
})

# ------------------------------------------------------------------------------
context("test-base-range")

test_that("returns same values as base R", {
  x <- rray(c(TRUE, FALSE), c(2, 2))
  expect_equal(range(x), range(vec_data(x)))
  expect_equal(range(x, x), range(vec_data(x), vec_data(x)))
  expect_equal(range(x, 5), range(vec_data(x), 5))
})

test_that("always uses `na.rm = TRUE`", {
  expect_equal(range(rray(c(NA, 1L)), na.rm = FALSE), c(1L, 1L))
})

# ------------------------------------------------------------------------------
context("test-base-prod")

test_that("returns same values as base R", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(prod(x), prod(vec_data(x)))
  expect_equal(prod(x, x), prod(vec_data(x), vec_data(x)))
})

test_that("broadcasts input using vctrs", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(prod(x, 5), prod(x, matrix(5, c(1, 2))))
})

test_that("always uses `na.rm = TRUE`", {
  expect_equal(prod(rray(c(NA, 1L)), na.rm = FALSE), 1)
})

# ------------------------------------------------------------------------------
context("test-base-sum")

test_that("returns same values as base R", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(sum(x), sum(vec_data(x)))
  expect_equal(sum(x, x), sum(vec_data(x), vec_data(x)))
})

test_that("broadcasts input using vctrs", {
  x <- rray(c(5, 6), c(2, 2))
  expect_equal(sum(x, 5), sum(x, matrix(5, c(1, 2))))
})

test_that("always uses `na.rm = TRUE`", {
  expect_equal(sum(rray(c(NA, 1L)), na.rm = FALSE), 1)
})

# ------------------------------------------------------------------------------
context("test-base-cummax")

test_that("vctrs dispatch works", {
  x <- rray(5:1)
  expect_equal(cummax(x), cummax(vec_data(x)))
})

test_that("flattens 2D+ arrays", {
  x <- rray(5:1, c(5, 1))
  expect_equal(cummax(x), rep(5L, 5))
})

test_that("keeps names if x is 1D", {
  x <- rray(5:1, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cummax(x)), rray_dim_names(x))
})

# ------------------------------------------------------------------------------
context("test-base-cummin")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cummin(x), cummin(vec_data(x)))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cummin(x), rep(1L, 5))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cummin(x)), rray_dim_names(x))
})

# ------------------------------------------------------------------------------
context("test-base-cumsum")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cumsum(x), cumsum(vec_data(x)))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cumsum(x), cumsum(1:5))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cumsum(x)), rray_dim_names(x))
})

test_that("integer overflow throws a warning", {
  x <- rray(c(2147483647L, 1L))
  expect_warning(cumsum(x), "integer overflow")
})

# ------------------------------------------------------------------------------
context("test-base-cumprod")

test_that("vctrs dispatch works", {
  x <- rray(1:5)
  expect_equal(cumprod(x), cumprod(vec_data(x)))
})

test_that("flattens 2D+ arrays", {
  x <- rray(1:5, c(5, 1))
  expect_equal(cumprod(x), cumprod(1:5))
})

test_that("keeps names if x is 1D", {
  x <- rray(1:5, dim_names = list(letters[1:5]))
  expect_equal(rray_dim_names(cumprod(x)), rray_dim_names(x))
})

test_that("a double is returned so no integer overflow occurs", {
  x <- rray(c(2147483647L, 2L))
  expect_equal(storage.mode(cumprod(x)), "double")
})
