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
