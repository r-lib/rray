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
