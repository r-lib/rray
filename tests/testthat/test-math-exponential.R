context("test-exp")

test_that("vctrs dispatch works", {
  expect_equal(exp(rray(1)), rray(exp(1)))
  expect_equal(exp(rray(1L)), rray(exp(1L)))
})

# ------------------------------------------------------------------------------
context("test-expm1")

test_that("vctrs dispatch works", {
  expect_equal(expm1(rray(1)), rray(expm1(1)))
  expect_equal(expm1(rray(1L)), rray(expm1(1L)))
})

# ------------------------------------------------------------------------------
context("test-log")

test_that("vctrs dispatch works", {
  expect_equal(log(rray(1)), rray(log(1)))
  expect_equal(log(rray(2), base = 2), rray(log(2, base = 2)))
})

# ------------------------------------------------------------------------------
context("test-log2")

test_that("vctrs dispatch works", {
  expect_equal(log2(rray(1)), rray(log2(1)))
})

# ------------------------------------------------------------------------------
context("test-log10")

test_that("vctrs dispatch works", {
  expect_equal(log10(rray(10)), rray(log10(10)))
})

# ------------------------------------------------------------------------------
context("test-log1p")

test_that("vctrs dispatch works", {
  expect_equal(log1p(rray(1)), rray(log1p(1)))
})

# ------------------------------------------------------------------------------
