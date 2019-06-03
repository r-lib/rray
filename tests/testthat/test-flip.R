context("test-flip")

test_that("flipping works", {

  x <- rray(as.double(1:6), c(3, 2))

  expect_equal(
    rray_flip(x, 1),
    rray(c(3,2,1,6,5,4), c(3,2))
  )

  expect_equal(
    rray_flip(x, 2),
    rray(c(4,5,6,1,2,3), c(3,2))
  )

})

test_that("flipping flips dimension names", {

  x <- rray(1:10, c(5, 2))
  x <- rray_set_row_names(x, letters[1:5])
  x <- rray_set_col_names(x, c("c1", "c2"))

  expect_equal(
    rray_row_names(rray_flip(x, 1)),
    rev(letters[1:5])
  )

  expect_equal(
    rray_col_names(rray_flip(x, 2)),
    c("c2", "c1")
  )

})

test_that("can flip base types", {

  x <- matrix(1:10, ncol = 2)
  x <- rray_set_row_names(x, letters[1:5])
  x <- rray_set_col_names(x, c("c1", "c2"))

  y <- array(1:12, c(2, 2, 3))
  y <- rray_set_axis_names(y, 3, c("d1", "d2", "d3"))

  expect_is(rray_flip(x, 1), "matrix")

  expect_equal(
    rray_row_names(rray_flip(x, 1)),
    rev(rray_row_names(x))
  )

  expect_equal(
    as.vector(rray_flip(x, 1)),
    c(5:1, 10:6)
  )

  expect_equal(
    as.vector(rray_flip(x, 2)),
    c(6:10, 1:5)
  )

  expect_equal(
    rray_dim_names(rray_flip(y, 3))[[3]],
    rev(rray_dim_names(y)[[3]])
  )

  expect_equal(
    as.vector(rray_flip(y, 3)),
    c(9:12, 5:8, 1:4)
  )

})

test_that("can flip with NULL", {
  expect_equal(rray_flip(NULL, 1), NULL)
})
