test_that("can split along a single axis", {
  x <- array(1:24, c(4, 3, 2))

  out <- rray_split(x, 3)
  expect_length(out, 2)
  expect_equal(dim(out[[1]]), c(4L, 3L, 1L))
  expect_equal(dim(out[[2]]), c(4L, 3L, 1L))
  expect_equal(out[[1]], array(1:12, c(4, 3, 1)))
  expect_equal(out[[2]], array(13:24, c(4, 3, 1)))

  out <- rray_split(x, 1)
  expect_length(out, 4)
  expect_equal(dim(out[[1]]), c(1L, 3L, 2L))
  expect_equal(out[[1]], array(x[1, , ], c(1, 3, 2)))

  out <- rray_split(x, 2)
  expect_length(out, 3)
  expect_equal(dim(out[[1]]), c(4L, 1L, 2L))
  expect_equal(out[[1]], array(x[, 1, ], c(4, 1, 2)))
})

test_that("can split along multiple axes", {
  x <- array(1:24, c(4, 3, 2))

  out <- rray_split(x, c(1, 2))
  expect_length(out, 12)
  expect_equal(dim(out[[1]]), c(1L, 1L, 2L))

  out <- rray_split(x, c(1, 2, 3))
  expect_length(out, 24)
  expect_equal(dim(out[[1]]), c(1L, 1L, 1L))
})

test_that("splitting with integer(0) axes returns list(x)", {
  x <- array(1:6, c(2, 3))
  out <- rray_split(x, integer())
  expect_equal(out, list(x))
})

test_that("dimension names on non-split axes are preserved", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 1)
  expect_equal(dimnames(out[[1]]), list("a", c("x", "y", "z")))
  expect_equal(dimnames(out[[2]]), list("b", c("x", "y", "z")))

  out <- rray_split(x, 2)
  expect_equal(dimnames(out[[1]]), list(c("a", "b"), "x"))
  expect_equal(dimnames(out[[2]]), list(c("a", "b"), "y"))
  expect_equal(dimnames(out[[3]]), list(c("a", "b"), "z"))
})

test_that("dimension names on split axes are subset", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, c(1, 2))
  expect_equal(dimnames(out[[1]]), list("a", "x"))
  expect_equal(dimnames(out[[2]]), list("b", "x"))
  expect_equal(dimnames(out[[3]]), list("a", "y"))
  expect_equal(dimnames(out[[4]]), list("b", "y"))
  expect_equal(dimnames(out[[5]]), list("a", "z"))
  expect_equal(dimnames(out[[6]]), list("b", "z"))
})

test_that("NULL dimension names are handled", {
  x <- array(1:6, c(2, 3))

  out <- rray_split(x, 1)
  expect_null(dimnames(out[[1]]))
})

test_that("partial dimension names are handled", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), NULL))

  out <- rray_split(x, 1)
  expect_equal(dimnames(out[[1]]), list("a", NULL))
  expect_equal(dimnames(out[[2]]), list("b", NULL))

  out <- rray_split(x, 2)
  expect_equal(dimnames(out[[1]]), list(c("a", "b"), NULL))
})

test_that("works with 1D arrays", {
  x <- array(1:5)
  out <- rray_split(x, 1)
  expect_length(out, 5)
  expect_equal(out[[1]], array(1L))
  expect_equal(out[[5]], array(5L))
})

test_that("works with different types", {
  expect_length(rray_split(array(c(TRUE, FALSE), c(1, 2)), 2), 2)
  expect_length(rray_split(array(c(1.5, 2.5), c(1, 2)), 2), 2)
  expect_length(rray_split(array(c("a", "b"), c(1, 2)), 2), 2)
  expect_length(rray_split(array(as.raw(1:2), c(1, 2)), 2), 2)
})

test_that("axes are validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_split(x, 3), error = TRUE)
  expect_snapshot(rray_split(x, 0), error = TRUE)
  expect_snapshot(rray_split(x, c(1, 1)), error = TRUE)
  expect_snapshot(rray_split(x, c(2, 1)), error = TRUE)
})
