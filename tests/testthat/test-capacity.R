test_that("returns length for non-arrays", {
  expect_identical(rray_capacity(1), 1)
  expect_identical(rray_capacity(1:5), 5)
  expect_identical(rray_capacity("a"), 1)
  expect_identical(rray_capacity(TRUE), 1)
})

test_that("returns product of dim for arrays", {
  expect_identical(rray_capacity(array(1, 2)), 2)
  expect_identical(rray_capacity(array(1, c(2, 3))), 6)
  expect_identical(rray_capacity(array(1, c(2, 3, 4))), 24)
})

test_that("returns product of dim for matrices", {
  expect_identical(rray_capacity(matrix(1, 2, 3)), 6)
})

test_that("returns 0 for empty vectors", {
  expect_identical(rray_capacity(integer()), 0)
  expect_identical(rray_capacity(character()), 0)
})

test_that("returns 0 for arrays with a zero dimension", {
  expect_identical(rray_capacity(array(integer(), c(0, 3))), 0)
  expect_identical(rray_capacity(array(integer(), c(2, 0, 4))), 0)
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_capacity(NULL), error = TRUE)
  expect_snapshot(rray_capacity(mean), error = TRUE)
})
