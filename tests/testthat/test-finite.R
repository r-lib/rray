# ------------------------------------------------------------------------------
context("test-is-nan")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, TRUE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.nan(x), expect)
})

# ------------------------------------------------------------------------------
context("test-is-finite")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(TRUE, FALSE, TRUE), c(3, 1), dim_names = nms)
  expect_equal(is.finite(x), expect)

  y <- rray(c(1, Inf, 2), c(3, 1), dim_names = nms)
  expect_equal(is.finite(y), expect)
})

# ------------------------------------------------------------------------------
context("test-is-infinite")

test_that("vctrs dispatch works", {
  nms <- list(NULL, "c1")
  x <- rray(c(1, NaN, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, FALSE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.infinite(x), expect)

  y <- rray(c(1, Inf, 2), c(3, 1), dim_names = nms)
  expect <- rray(c(FALSE, TRUE, FALSE), c(3, 1), dim_names = nms)
  expect_equal(is.infinite(y), expect)
})
