test_that("returns 1 for non-arrays", {
  expect_identical(rray_dimensionality(1), 1L)
  expect_identical(rray_dimensionality(1:5), 1L)
  expect_identical(rray_dimensionality("a"), 1L)
  expect_identical(rray_dimensionality(TRUE), 1L)
})

test_that("returns dimensionality for arrays", {
  expect_identical(rray_dimensionality(array(1, 2)), 1L)
  expect_identical(rray_dimensionality(array(1, c(2, 3))), 2L)
  expect_identical(rray_dimensionality(array(1, c(2, 3, 4))), 3L)
  expect_identical(rray_dimensionality(array(1, c(2, 3, 4, 5))), 4L)
})

test_that("returns 2 for matrices", {
  expect_identical(rray_dimensionality(matrix(1, 2, 3)), 2L)
})

test_that("errors on NULL", {
  expect_snapshot(rray_dimensionality(NULL), error = TRUE)
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_dimensionality(mean), error = TRUE)
  expect_snapshot(rray_dimensionality(quote(x)), error = TRUE)
  expect_snapshot(rray_dimensionality(environment()), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_dimensionality(x), error = TRUE)
})
