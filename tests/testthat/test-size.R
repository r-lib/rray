test_that("returns length for non-arrays", {
  expect_identical(rray_size(1), 1)
  expect_identical(rray_size(1:5), 5)
  expect_identical(rray_size("a"), 1)
  expect_identical(rray_size(TRUE), 1)
})

test_that("returns product of dim for arrays", {
  expect_identical(rray_size(array(1, 2)), 2)
  expect_identical(rray_size(array(1, c(2, 3))), 6)
  expect_identical(rray_size(array(1, c(2, 3, 4))), 24)
})

test_that("returns product of dim for matrices", {
  expect_identical(rray_size(matrix(1, 2, 3)), 6)
})

test_that("returns 0 for empty vectors", {
  expect_identical(rray_size(integer()), 0)
  expect_identical(rray_size(character()), 0)
})

test_that("returns 0 for arrays with a zero dimension", {
  expect_identical(rray_size(array(integer(), c(0, 3))), 0)
  expect_identical(rray_size(array(integer(), c(2, 0, 4))), 0)
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_size(NULL), error = TRUE)
  expect_snapshot(rray_size(mean), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_size(x), error = TRUE)
})
