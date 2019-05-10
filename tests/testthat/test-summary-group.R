# ------------------------------------------------------------------------------
context("test-xtfrm")

test_that("xtfrm() returns proxy objects", {
  x <- rray(1:2, c(2, 1))
  expect_equal(xtfrm(x), vec_data(x))

  x <- rray(c(1, 2), c(2, 1))
  expect_equal(xtfrm(x), vec_data(x))
})

test_that("xtfrm() works for 3D", {
  x <- rray(1:6, c(2, 1, 3))
  expect_equal(xtfrm(x), vec_data(x))
})

test_that("xtfrm() for logicals returns integers", {
  x <- rray(c(TRUE, FALSE), c(2, 1))
  expect_equal(xtfrm(x), new_matrix(c(1, 0), c(2, 1)))
})

# ------------------------------------------------------------------------------
context("test-min")

test_that("`min()` returns a length 1 vector for 1D", {
  expect_equal(min(rray(5:1)), rray(1L))
  expect_equal(min(rray(5:1 + 0)), rray(1))
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

# TODO - Add tests after this is fixed
# https://github.com/r-lib/vctrs/pull/329

# test_that("NAs are removed", {
#   min(rray(c(NA, 2)), na.rm = TRUE)
# })

# ------------------------------------------------------------------------------
context("test-max")

test_that("`max()` returns a length 1 vector for 1D", {
  expect_equal(max(rray(5:1)), rray(5L))
  expect_equal(max(rray(5:1 + 0)), rray(5))
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

# TODO - Add tests after this is fixed
# https://github.com/r-lib/vctrs/pull/329

# test_that("NAs are removed", {
#   max(rray(c(NA, 2)), na.rm = TRUE)
# })

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
