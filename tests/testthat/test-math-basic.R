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

# ------------------------------------------------------------------------------

context("test-multiply-add")

test_that("rray_multiply_add() basics", {

  expect_equal(rray_multiply_add(1, 2, 1), new_array(3))
  expect_equal(rray_multiply_add(1L, 2L, 1L), new_array(3L))

  # broadcasting
  x <- matrix(1:5)
  expect_equal(vec_dim(rray_multiply_add(x, t(x), x)), c(5, 5))

  # with names
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_multiply_add(x, 1, 1),
    rray(c(1, 1), c(2, 1), list(c("r1", "r2"), "c1"))
  )

})

test_that("rray_multiply_add() corner cases", {

  # Logicals
  expect_equal(rray_multiply_add(TRUE, TRUE, TRUE), new_array(2L))
  expect_equal(rray_multiply_add(FALSE, FALSE, FALSE), new_array(0L))

  # NaN
  expect_equal(rray_multiply_add(NaN, NaN, NaN), as_array(NaN))

  # Inf
  expect_equal(rray_multiply_add(-Inf, -1, 1), as_array(Inf))
})

# ------------------------------------------------------------------------------
context("test-maximum")

test_that("basics", {
  expect_identical(rray_maximum(1, 2), new_array(2))
  expect_identical(rray_maximum(1L, 2L), new_array(2L))
})

test_that("broadcasting works", {
  x <- matrix(1:2)
  expect_equal(rray_maximum(x, t(x)), new_matrix(c(1, 2, 2, 2), c(2, 2)))
})

test_that("works with 3D", {
  x <- array(1:2, c(2, 1, 2))
  y <- matrix(2:1)
  expect_equal(rray_maximum(x, y), new_array(2, c(2, 1, 2)))
})

test_that("dimension names are kept", {
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_maximum(x, 1),
    rray(c(1, 1), c(2, 1), list(c("r1", "r2"), "c1"))
  )
})

# ------------------------------------------------------------------------------
context("test-minimum")

test_that("basics", {
  expect_identical(rray_minimum(1, 2), new_array(1))
  expect_identical(rray_minimum(1L, 2L), new_array(1L))
})

test_that("broadcasting works", {
  x <- matrix(1:2)
  expect_equal(rray_minimum(x, t(x)), new_matrix(c(1, 1, 1, 2), c(2, 2)))
})

test_that("works with 3D", {
  x <- array(1:2, c(2, 1, 2))
  y <- matrix(2:1)
  expect_equal(rray_minimum(x, y), new_array(1, c(2, 1, 2)))
})

test_that("dimension names are kept", {
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_minimum(x, 1),
    rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  )
})

# ------------------------------------------------------------------------------
context("test-clip")

test_that("basics", {
  expect_identical(rray_clip(1:3, 1, 2), new_array(c(1L, 2L, 2L)))
  expect_identical(
    rray_clip(rray(1:6, c(2, 3, 1)), 1, 3),
    rray(c(1L, 2L, rep(3L, 4)), c(2, 3, 1))
  )
})

test_that("dimension names are kept", {
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_clip(x, 1, 2),
    rray(1, c(2, 1), list(c("r1", "r2"), "c1"))
  )
})

test_that("cannot clip if `low` is greater than `high`", {
  expect_error(rray_clip(1, 2, 1), "less than or equal to")
})

test_that("`low` and `high` must be size 1", {
  expect_error(rray_clip(1, c(1, 1), 1), "1, not size 2", class = "vctrs_error_assert_size")
  expect_error(rray_clip(1, 1, c(1, 1)), "1, not size 2", class = "vctrs_error_assert_size")
})

test_that("cannot clip with NULL bounds", {
  expect_error(rray_clip(1, NULL, 1), "not NULL", class = "vctrs_error_scalar_type")
  expect_error(rray_clip(1, 1, NULL), "not NULL", class = "vctrs_error_scalar_type")
})

test_that("clip NULL", {
  expect_equal(rray_clip(NULL, 1, 1), NULL)
})

test_that("clip with length 0 x", {
  expect_equal(rray_clip(logical(), 1, 1), new_array(logical()))
  expect_equal(rray_clip(numeric(), 1, 1), new_array(numeric()))
})
