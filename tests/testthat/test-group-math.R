# ------------------------------------------------------------------------------
context("test-cummax")

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
context("test-cummin")

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
context("test-cumsum")

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
context("test-cumprod")

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
