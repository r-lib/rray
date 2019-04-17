context("test-det")

test_that("can compute the determinant of a matrix", {
  x <- matrix(c(2, 3, 4, 1), nrow = 2)
  expect_equal(as_matrix(det(x)), rray_det(x))
})

test_that("computing the determinant doesn't drop dimensionality", {
  x <- matrix(c(2, 3, 4, 1), nrow = 2)
  expect_equal(vec_dim(rray_det(x)), c(1, 1))
})

test_that("can compute the determinant of sub matrices in an array", {
  x <- array(c(2, 3, 4, 1, 2, 4, 6, 8), c(2, 2, 2))
  expect_equal(vec_dim(rray_det(x)), c(1, 1, 2))
  expect_equal(rray_det(x), new_array(c(-10, -8), c(1, 1, 2)))
})

