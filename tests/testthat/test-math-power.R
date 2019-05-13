context("test-square")

test_that("basic", {
  expect_equal(rray_square(rray(2)), rray(2 ^ 2))
  expect_equal(rray_square(rray(TRUE)), rray(TRUE ^ 2))
  expect_equal(rray_square(rray(matrix(1:5))), rray(matrix(1:5) ^ 2))
})

test_that("dim names are kept", {
  nms <- list("r1", "c1")
  x <- rray(1, c(1, 1), dim_names = nms)
  expect_equal(rray_dim_names(rray_square(x)), rray_dim_names(x))
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
  expect_equal(rray_dim_names(rray_cube(x)), rray_dim_names(x))
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
  expect_equal(rray_dim_names(rray_sqrt(x)), rray_dim_names(x))
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
  expect_equal(rray_dim_names(rray_cbrt(x)), rray_dim_names(x))
})

test_that("corner cases", {
  expect_equal(rray_cbrt(Inf), new_array(Inf))
  expect_equal(rray_cbrt(-Inf), new_array(-Inf))

  expect_equal(rray_cbrt(0L), new_array(0))

  expect_equal(rray_cbrt(NaN), new_array(NaN))
})

# ------------------------------------------------------------------------------
context("test-hypot")

test_that("basic", {
  expect_equal(rray_hypot(2, 3), new_array(sqrt(2^2 + 3^2)))
  expect_equal(rray_hypot(TRUE, TRUE), new_array(sqrt(1^2 + 1^2)))
})

test_that("broadcasting is performed", {
  expect_equal(
    rray_hypot(matrix(1:2), matrix(1:2, 1)),

    new_matrix(
      c(
        sqrt(1 ^ 2 + 1 ^ 2),
        sqrt(1 ^ 2 + 2 ^ 2),
        sqrt(2 ^ 2 + 1 ^ 2),
        sqrt(2 ^ 2 + 2 ^ 2)
      ),
      c(2, 2)
    )
  )
})

test_that("dimension names are kept", {
  x <- rray(1, dim = c(1, 1), dim_names = list("r1", NULL))
  y <- rray(2, dim = c(1, 1), dim_names = list("rr1", "cc1"))
  expect_equal(rray_dim_names(rray_hypot(x, y)), list("r1", "cc1"))
  expect_equal(rray_dim_names(rray_hypot(y, x)), rray_dim_names(y))
})

test_that("corner cases", {
  expect_equal(rray_hypot(Inf, 1), new_array(Inf))
  expect_equal(rray_hypot(-Inf, 1), new_array(Inf))

  expect_equal(rray_hypot(0, 0), new_array(0))

  expect_equal(rray_hypot(NaN, NaN), new_array(NaN))
})
