context("test-square")

test_that("basic", {
  expect_equal(rray_square(rray(2)), rray(2 ^ 2))
  expect_equal(rray_square(rray(TRUE)), rray(TRUE ^ 2))
  expect_equal(rray_square(rray(matrix(1:5))), rray(matrix(1:5) ^ 2))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_square(x)), dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_square(Inf), new_array(Inf))
  expect_equal(rray_square(-Inf), new_array(Inf))

  expect_equal(rray_square(0L), new_array(0))

  expect_equal(rray_square(NaN), new_array(NaN))
})

# ------------------------------------------------------------------------------
context("test-cube")

test_that("basic", {
  expect_equal(rray_cube(rray(2)), rray(2 ^ 3))
  expect_equal(rray_cube(rray(TRUE)), rray(TRUE ^ 3))
  expect_equal(rray_cube(rray(matrix(1:5))), rray(matrix(1:5) ^ 3))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_cube(x)), dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_cube(Inf), new_array(Inf))
  expect_equal(rray_cube(-Inf), new_array(-Inf))

  expect_equal(rray_cube(0L), new_array(0))

  expect_equal(rray_cube(NaN), new_array(NaN))
})

# ------------------------------------------------------------------------------
context("test-sqrt")

test_that("basic", {
  expect_equal(rray_sqrt(rray(2)), rray(2 ^ (1/2)))
  expect_equal(rray_sqrt(rray(TRUE)), rray(TRUE ^ (1/2)))
  expect_equal(rray_sqrt(rray(matrix(1:5))), rray(matrix(1:5) ^ (1/2)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_sqrt(x)), dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_sqrt(Inf), new_array(Inf))
  expect_equal(rray_sqrt(-Inf), new_array(NaN))

  expect_equal(rray_sqrt(0L), new_array(0))

  expect_equal(rray_sqrt(NaN), new_array(NaN))
})

test_that("vctrs dispatch works", {
  expect_equal(sqrt(rray(1)), rray_sqrt(rray(1)))
  expect_equal(sqrt(rray(1L)), rray_sqrt(rray(1L)))
})

# ------------------------------------------------------------------------------
context("test-cbrt")

test_that("basic", {
  expect_equal(rray_cbrt(rray(2)), rray(2 ^ (1/3)))
  expect_equal(rray_cbrt(rray(TRUE)), rray(TRUE ^ (1/3)))
  expect_equal(rray_cbrt(rray(matrix(1:5))), rray(matrix(1:5) ^ (1/3)))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(dim_names(rray_cbrt(x)), dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_cbrt(Inf), new_array(Inf))
  expect_equal(rray_cbrt(-Inf), new_array(-Inf))

  expect_equal(rray_cbrt(0L), new_array(0))

  expect_equal(rray_cbrt(NaN), new_array(NaN))
})
