# ------------------------------------------------------------------------------
# rray_reduce_names()

test_that("rray_reduce_names() keeps names of axes that aren't reduced", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(rray_reduce_names(x, 1L), list(NULL, c("c1", "c2", "c3")))
  expect_identical(rray_reduce_names(x, 2L), list(c("r1", "r2"), NULL))
})

test_that("rray_reduce_names() drops names of every reduced axis", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_null(rray_reduce_names(x, c(1L, 2L)))
})

test_that("rray_reduce_names() keeps everything when nothing is reduced", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_reduce_names(x, integer()),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_reduce_names() drops names of a reduced axis of dimension 1", {
  x <- array(1:3, c(1L, 3L), dimnames = list("only", c("c1", "c2", "c3")))
  expect_identical(rray_reduce_names(x, 1L), list(NULL, c("c1", "c2", "c3")))
})

test_that("rray_reduce_names() returns NULL when `x` has no names", {
  x <- array(1:6, c(2L, 3L))
  expect_null(rray_reduce_names(x, 1L))
})

test_that("rray_reduce_names() leaves unnamed axes alone", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(rray_reduce_names(x, 2L), list(c("r1", "r2"), NULL))
  expect_null(rray_reduce_names(x, 1L))
})

test_that("rray_reduce_names() works with 1 dimensional arrays", {
  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_null(rray_reduce_names(x, 1L))
  expect_identical(rray_reduce_names(x, integer()), list(c("a", "b")))
})

test_that("rray_reduce_names() works with 3+ dimensional arrays", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), letters[1:4])
  )
  expect_identical(
    rray_reduce_names(x, 2L),
    list(c("r1", "r2"), NULL, letters[1:4])
  )
  expect_identical(
    rray_reduce_names(x, c(1L, 3L)),
    list(NULL, c("c1", "c2", "c3"), NULL)
  )
})

test_that("rray_reduce_names() works with zero-size axes", {
  x <- array(integer(), c(0L, 3L), dimnames = list(NULL, c("c1", "c2", "c3")))
  expect_identical(rray_reduce_names(x, 1L), list(NULL, c("c1", "c2", "c3")))
  expect_null(rray_reduce_names(x, 2L))
})
