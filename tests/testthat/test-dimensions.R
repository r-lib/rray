test_that("returns length for non-arrays", {
  expect_identical(rray_dimensions(1), 1L)
  expect_identical(rray_dimensions(1:5), 5L)
  expect_identical(rray_dimensions("a"), 1L)
  expect_identical(rray_dimensions(TRUE), 1L)
})

test_that("returns dim for arrays", {
  expect_identical(rray_dimensions(array(1, 2)), 2L)
  expect_identical(rray_dimensions(array(1, c(2, 3))), c(2L, 3L))
  expect_identical(rray_dimensions(array(1, c(2, 3, 4))), c(2L, 3L, 4L))
})

test_that("returns dim for matrices", {
  expect_identical(rray_dimensions(matrix(1, 2, 3)), c(2L, 3L))
})

test_that("returns length for empty vectors", {
  expect_identical(rray_dimensions(integer()), 0L)
  expect_identical(rray_dimensions(character()), 0L)
})

test_that("errors on NULL", {
  expect_snapshot(rray_dimensions(NULL), error = TRUE)
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_dimensions(mean), error = TRUE)
  expect_snapshot(rray_dimensions(quote(x)), error = TRUE)
  expect_snapshot(rray_dimensions(environment()), error = TRUE)
})
