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
  x <- set_row_names(x, letters[1:5])
  x <- set_col_names(x, c("c1", "c2"))

  expect_equal(
    row_names(rray_flip(x, 1)),
    rev(letters[1:5])
  )

  expect_equal(
    col_names(rray_flip(x, 2)),
    c("c2", "c1")
  )

})
