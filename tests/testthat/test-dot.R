test_that("matrix multiplication works but the class is lost", {
  x <- rray(1:2)
  expect_equal(x %*% x, matrix(5))
})

test_that("The class is preserved", {
  x <- rray(1:2)
  expect_equal(rray_dot(x, x), rray(5, c(1, 1)))
})

test_that("Error if input is >2D", {
  x <- array(1, c(1, 1, 1))
  expect_error(rray_dot(x, 1), "1 or 2, not 3")
  expect_error(rray_dot(1, x), "1 or 2, not 3")
})
