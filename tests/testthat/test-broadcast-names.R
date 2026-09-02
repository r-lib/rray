# ------------------------------------------------------------------------------
# rray_broadcast_names()

test_that("rray_broadcast_names() keeps names of axes that don't change", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_broadcast_names(x, c(2L, 3L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names() drops names of axes that change", {
  x <- array(1:3, c(1L, 3L), dimnames = list("only", c("c1", "c2", "c3")))
  expect_identical(
    rray_broadcast_names(x, c(2L, 3L)),
    list(NULL, c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names() keeps names of an axis broadcast from 1 to 1", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  expect_identical(rray_broadcast_names(x, c(1L, 3L)), list("a", NULL))
})

test_that("rray_broadcast_names() returns NULL when `x` has no names", {
  x <- array(1:6, c(2L, 3L))
  expect_null(rray_broadcast_names(x, c(2L, 3L)))
})

test_that("rray_broadcast_names() returns NULL when every named axis changes", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  expect_null(rray_broadcast_names(x, c(2L, 2L)))
})

test_that("rray_broadcast_names() leaves new trailing axes unnamed", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_broadcast_names(x, c(2L, 3L, 4L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"), NULL)
  )
})

test_that("rray_broadcast_names() works with 1 dimensional arrays", {
  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_identical(rray_broadcast_names(x, 2L), list(c("a", "b")))
  expect_identical(rray_broadcast_names(x, c(2L, 3L)), list(c("a", "b"), NULL))
})

test_that("rray_broadcast_names() works with zero-size axes", {
  x <- array(integer(), c(0L, 3L), dimnames = list(NULL, c("c1", "c2", "c3")))
  expect_identical(
    rray_broadcast_names(x, c(0L, 3L)),
    list(NULL, c("c1", "c2", "c3"))
  )
})

# ------------------------------------------------------------------------------
# rray_broadcast_names2()

test_that("rray_broadcast_names2() prefers `x` where its dimension is kept", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() falls through to `y` where `x` broadcasts", {
  x <- array(1:3, c(1L, 3L), dimnames = list("only", c("c1", "c2", "c3")))
  y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("a", "b"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() falls through to `y` on an unnamed axis", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("r1", "r2"), NULL))
  y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("r1", "r2"), c("d", "e", "g"))
  )
})

test_that("rray_broadcast_names2() takes each axis from a different input", {
  x <- array(1:3, c(3L, 1L), dimnames = list(c("r1", "r2", "r3"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list(NULL, c("c1", "c2")))
  expect_identical(
    rray_broadcast_names2(x, y, c(3L, 2L)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
})

test_that("rray_broadcast_names2() returns NULL when neither input has names", {
  x <- array(1:6, c(2L, 3L))
  y <- array(1:6, c(2L, 3L))
  expect_null(rray_broadcast_names2(x, y, c(2L, 3L)))
})

test_that("rray_broadcast_names2() returns NULL when every axis broadcasts", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  y <- array(1L, c(1L, 1L), dimnames = list("c", "d"))
  expect_null(rray_broadcast_names2(x, y, c(2L, 2L)))
})

test_that("rray_broadcast_names2() works with differing dimensionality", {
  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  y <- array(1:24, c(2L, 3L, 4L))
  y <- rray_set_axis_names(y, 3L, letters[1:4])
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L, 4L)),
    list(c("a", "b"), NULL, letters[1:4])
  )
})

# ------------------------------------------------------------------------------
# rray_broadcast_names_common()

test_that("rray_broadcast_names_common() lets the first input to keep an axis win", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names_common(x, y, .dimensions = c(2L, 3L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names_common() takes each axis from a different input", {
  x <- array(1L, c(1L, 1L, 1L), dimnames = list("a", NULL, NULL))
  y <- array(1:2, c(1L, 2L, 1L), dimnames = list(NULL, c("b", "c"), NULL))
  z <- array(1:3, c(1L, 1L, 3L), dimnames = list(NULL, NULL, c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names_common(x, y, z, .dimensions = c(1L, 2L, 3L)),
    list("a", c("b", "c"), c("d", "e", "g"))
  )
})

test_that("rray_broadcast_names_common() skips inputs whose dimension changed", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  y <- array(1:2, c(2L, 1L), dimnames = list(c("c", "d"), "e"))
  expect_identical(
    rray_broadcast_names_common(x, y, .dimensions = c(2L, 2L)),
    list(c("c", "d"), NULL)
  )
})

test_that("rray_broadcast_names_common() returns NULL when no input has names", {
  x <- array(1:6, c(2L, 3L))
  y <- array(1:3, c(1L, 3L))
  expect_null(rray_broadcast_names_common(x, y, .dimensions = c(2L, 3L)))
})

test_that("rray_broadcast_names_common() with one input matches rray_broadcast_names()", {
  x <- array(1:3, c(1L, 3L), dimnames = list("only", c("c1", "c2", "c3")))
  expect_identical(
    rray_broadcast_names_common(x, .dimensions = c(2L, 3L)),
    rray_broadcast_names(x, c(2L, 3L))
  )
})

test_that("rray_broadcast_names_common() returns NULL for empty dots", {
  expect_null(rray_broadcast_names_common(.dimensions = c(2L, 3L)))
})
