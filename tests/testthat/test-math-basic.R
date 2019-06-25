context("test-abs")

test_that("vctrs dispatch works", {
  expect_equal(abs(rray(TRUE)), rray(abs(TRUE)))
  expect_equal(abs(rray(1L)), rray(abs(1L)))
})

# ------------------------------------------------------------------------------
context("test-sign")

test_that("vctrs dispatch works", {
  expect_equal(sign(rray(TRUE)), rray(sign(TRUE)))
  expect_equal(sign(rray(1L)), rray(sign(1L)))
})
