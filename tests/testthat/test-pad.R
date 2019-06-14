context("test-pad")

test_that("pad structure", {
  expect_is(pad(), "vctrs_pad")
})

test_that("can pad the front side", {
  x <- rray(1:4, c(1, 1, 2, 2))
  expect_equal(x[pad(), 1], x[, , , 1])
  expect_equal(x[pad(), 1, 1], x[, , 1, 1])
  expect_equal(x[pad(), 1, 1], x[, , 1, 1])
})

test_that("can pad the middle", {
  x <- rray(1:8, c(2, 1, 2, 2))
  expect_equal(x[1, pad(), 1], x[1, , , 1])
  expect_equal(x[1, pad(), 1, 1], x[1, , 1, 1])
})

test_that("pad() is the same as asking for the entire array", {
  x <- rray(1:8, c(2, 1, 2, 2))
  expect_equal(x[pad()], x[])
  expect_equal(x[pad()], x)
})

test_that("padding is ignored if not required", {
  x <- rray(1:8, c(2, 1, 2, 2))
  expect_equal(x[pad(), 1, 1, 1, 1], x[1, 1, 1, 1])
  expect_equal(x[1, pad(), 1, 1, 1], x[1, 1, 1, 1])
  expect_equal(x[1, 1, 1, 1, pad()], x[1, 1, 1, 1])
})

test_that("pad() allows dimensionality errors through correctly", {
  x <- rray(1:8, c(2, 1, 2, 2))
  expect_error(x[pad(), 1, 1, 1, 1, 1], "into dimension 5")
  expect_error(x[1, pad(), 1, 1, 1, 1], "into dimension 5")
  expect_error(x[1, 1, 1, 1, 1, 1, pad()], "into dimension 6")
})

test_that("can pad() base R", {
  x <- new_array(1:8, c(2, 1, 2, 2))
  expect_equal(rray_subset(x, pad(), 1), x[, , , 1, drop = FALSE])
})

test_that("cannot have more than 1 pad()", {
  x <- rray(1:8, c(2, 1, 2, 2))
  expect_error(x[pad(), pad()], "Only one")
  expect_error(x[pad(), 1, pad()], "Only one")
  expect_error(x[1, 1, pad(), pad()], "Only one")
})

test_that("pad format", {
  expect_equal(format(pad()), "<padding>")
})
