context("test-full-like")

test_that("can perform a basic fill", {
  x <- array(1:10)
  expect_equal(
    rray_full_like(x, 1L),
    array(rep(1L, 10))
  )
})

test_that("rray class is retained", {
  x <- rray(1:10, c(5, 2))
  expect_equal(
    rray_full_like(x, 0),
    rray(0L, c(5, 2))
  )
})

test_that("can catch `value` with length >1", {
  expect_error(rray_full_like(1, c(1, 2)), "1, not 2")
})

test_that("can coerce `value` as needed", {
  expect_is(
    rray_full_like(rray(1), 1L),
    "vctrs_rray_dbl"
  )

  expect_is(
    rray_full_like(rray(1), TRUE),
    "vctrs_rray_dbl"
  )

  expect_error(rray_full_like(rray(1), "foo"), class = "vctrs_error_cast_lossy")
})

test_that("character `value` is supported", {
  expect_equal(rray_full_like(rray(1), "2"), rray(2))
})

test_that("arrays of >1 dimensionality are supported", {
  expect_equal(
    rray_full_like(rray(c(TRUE, FALSE)), array(1, c(1, 1))),
    rray(c(TRUE, TRUE))
  )

  expect_equal(
    rray_full_like(rray(c(TRUE, FALSE)), array(1, c(1, 1, 1))),
    rray(c(TRUE, TRUE))
  )
})

test_that("can fill 3D", {
  expect_equal(
    rray_full_like(rray(1:12, c(2, 3, 2)), 0),
    rray(0L, c(2, 3, 2))
  )
})

test_that("`NULL` input returns `NULL`", {
  expect_equal(rray_full_like(NULL, 1), NULL)
})

test_that("0-length input is supported", {
  expect_equal(rray_full_like(numeric(), 1), array(numeric()))
})

test_that("ones of different types are generated", {
  expect_identical(rray_ones_like(rray(2L)), rray(1L))
  expect_identical(rray_ones_like(rray(2)), rray(1))
  expect_identical(rray_ones_like(rray(FALSE)), rray(TRUE))
})

test_that("zeroes of different types are generated", {
  expect_identical(rray_zeros_like(rray(2L)), rray(0L))
  expect_identical(rray_zeros_like(rray(2)), rray(0))
  expect_identical(rray_zeros_like(rray(FALSE)), rray(FALSE))
})
