context("test-ceiling")

test_that("vctrs dispatch works", {
  expect_equal(ceiling(rray(1)), rray(ceiling(1)))
})

# ------------------------------------------------------------------------------
context("test-floor")

test_that("vctrs dispatch works", {
  expect_equal(floor(rray(1)), rray(floor(1)))
})

# ------------------------------------------------------------------------------
context("test-trunc")

test_that("vctrs dispatch works", {
  expect_equal(trunc(rray(1)), rray(trunc(1)))
})

# ------------------------------------------------------------------------------
context("test-round")

test_that("vctrs dispatch works", {
  expect_equal(round(rray(1)), rray(round(1)))
  expect_equal(round(rray(1.5), digits = 0), rray(round(1.5, digits = 0)))
})

# ------------------------------------------------------------------------------
context("test-signif")

test_that("vctrs dispatch works", {
  expect_equal(signif(rray(1)), rray(signif(1)))
  expect_equal(signif(rray(1.5), digits = 1), rray(signif(1.5, digits = 1)))
})
