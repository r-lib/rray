context("test-transpose")

test_that("can transpose a base R matrix with names", {

  x <- matrix(1:6, c(3, 2), dimnames = list(letters[1:3], letters[4:5]))

  expect_equal(
    rray_transpose(x),
    t(x)
  )
})

test_that("can transpose a 3D array with names", {

  x <- new_array(1:2, c(2, 1, 2), dimnames = list(letters[1:2], letters[3], letters[4:5]))

  expect_equal(
    rray_transpose(x),
    aperm(x)
  )

})

test_that("transposing a 1D array does nothing", {
  expect_equal(
    rray_transpose(1:5),
    new_array(1:5)
  )
})

test_that("t() method for rray objects follows base behavior", {

  x <- rray(1:5)

  expect_equal(
    rray(1:5, c(1, 5)),
    t(x)
  )

})

test_that("t() errors on >2D", {
  xx <- rray(1:6, c(3, 2, 1))
  expect_error(t(xx), "do you need `rray_transpose()`?")
})

test_that("aperm() method for rray objects", {

  x <- rray(1:5)

  expect_equal(
    rray_transpose(x),
    aperm(x)
  )

  xx <- rray(1:6, c(3, 2, 1))

  expect_equal(
    rray_transpose(xx),
    aperm(xx)
  )

  expect_equal(
    rray_transpose(xx, c(2, 1, 3)),
    aperm(xx, c(2, 1, 3))
  )
})

test_that("validate permutation", {
  expect_error(rray_transpose(1, "hi"))
  expect_error(rray_transpose(1, 2), "maximum value for `permutation` is 1")
  expect_error(rray_transpose(1, c(1, 2)), "must have size 1 to permute `x`")
  expect_error(rray_transpose(matrix(1), c(1, 1)), "more than once: 1")
})
