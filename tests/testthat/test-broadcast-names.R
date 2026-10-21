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

test_that("rray_broadcast_names() keeps titles, including on broadcast axes", {
  x <- array(1L, c(1L, 1L), dimnames = list(A = "a", B = "b"))
  expect_identical(
    rray_broadcast_names(x, c(2L, 1L, 3L)),
    list(A = NULL, B = "b", NULL)
  )
})

test_that("rray_broadcast_names() keeps titles when `x` has no names", {
  x <- array(1:6, c(2L, 3L), dimnames = list(A = NULL, B = NULL))
  expect_identical(
    rray_broadcast_names(x, c(2L, 3L)),
    list(A = NULL, B = NULL)
  )
})

test_that("rray_broadcast_names() keeps titles that are all empty", {
  x <- rray_set_titles(array(1:6, c(2L, 3L)), c("", ""))
  expect_identical(
    rray_broadcast_names(x, c(2L, 3L)),
    structure(list(NULL, NULL), names = c("", ""))
  )
})

# ------------------------------------------------------------------------------
# rray_broadcast_names2()

test_that("rray_broadcast_names2() keeps names that agree", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  y <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() errors on names that disagree", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  y <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c9", "c3"))
  )
  expect_snapshot(error = TRUE, rray_broadcast_names2(x, y, c(2L, 3L)))
})

test_that("rray_broadcast_names2() treats empty names like any other name", {
  x <- array(1:2, 2L, dimnames = list(c("r1", "")))
  y <- array(1:2, 2L, dimnames = list(c("r1", "r2")))
  expect_identical(rray_broadcast_names2(x, x, 2L), list(c("r1", "")))
  expect_snapshot(error = TRUE, rray_broadcast_names2(x, y, 2L))
})

test_that("rray_broadcast_names2() only matches `NA` with `NA`", {
  x <- array(1:2, 2L, dimnames = list(c("r1", NA)))
  y <- array(1:2, 2L, dimnames = list(c("r1", "NA")))
  expect_identical(rray_broadcast_names2(x, x, 2L), list(c("r1", NA)))
  expect_snapshot(error = TRUE, {
    rray_broadcast_names2(x, y, 2L)
    rray_broadcast_names2(y, x, 2L)
  })
})

test_that("rray_broadcast_names2() falls through to `y` where `x` broadcasts", {
  x <- array(1:3, c(1L, 3L), dimnames = list("only", c("c1", "c2", "c3")))
  y <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("a", "b"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() falls through to `y` on an unnamed axis", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("r1", "r2"), NULL))
  y <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("d", "e", "g"))
  )
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

test_that("rray_broadcast_names2() keeps titles that agree", {
  x <- array(1:2, 2L, dimnames = list(A = c("a", "b")))
  y <- array(1:2, 2L, dimnames = list(A = NULL))
  expect_identical(rray_broadcast_names2(x, y, 2L), list(A = c("a", "b")))
})

test_that("rray_broadcast_names2() errors on titles that disagree", {
  x <- array(1:2, 2L, dimnames = list(A = c("a", "b")))
  y <- array(1:2, 2L, dimnames = list(B = c("a", "b")))
  expect_snapshot(error = TRUE, rray_broadcast_names2(x, y, 2L))
})

test_that("rray_broadcast_names2() gives empty titles no say", {
  x <- array(1:4, c(2L, 2L), dimnames = list(A = NULL, NULL))
  y <- array(1:4, c(2L, 2L), dimnames = list(NULL, B = NULL))
  z <- array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 2L)),
    list(A = NULL, B = NULL)
  )
  expect_identical(
    rray_broadcast_names2(z, x, c(2L, 2L)),
    list(A = c("a", "b"), NULL)
  )
})

test_that("rray_broadcast_names2() checks titles where an input broadcasts", {
  x <- array(1L, c(1L, 2L), dimnames = list(A = "total", NULL))
  y <- array(1:4, c(2L, 2L), dimnames = list(B = c("a", "b"), NULL))
  expect_snapshot(error = TRUE, rray_broadcast_names2(x, y, c(2L, 2L)))
})

test_that("rray_broadcast_names2() keeps titles where an input broadcasts", {
  x <- array(1L, c(1L, 2L), dimnames = list(A = "total", NULL))
  y <- array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 2L)),
    list(A = c("a", "b"), NULL)
  )
})

test_that("rray_broadcast_names2() only matches `NA` titles with `NA`", {
  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  names(dimnames(x)) <- NA_character_
  y <- array(1:2, 2L, dimnames = list("NA" = c("a", "b")))
  expect_identical(rray_titles(x), NA_character_)
  expect_identical(
    rray_broadcast_names2(x, x, 2L),
    structure(list(c("a", "b")), names = NA_character_)
  )
  expect_snapshot(error = TRUE, {
    rray_broadcast_names2(x, y, 2L)
    rray_broadcast_names2(y, x, 2L)
  })
})

test_that("rray_broadcast_names2() checks names before titles", {
  x <- array(1:2, 2L, dimnames = list(A = c("a", "b")))
  y <- array(1:2, 2L, dimnames = list(B = c("c", "d")))
  expect_snapshot(error = TRUE, rray_broadcast_names2(x, y, 2L))
})

test_that("rray_broadcast_names2() gives a missing axis no title", {
  x <- array(1:2, 2L, dimnames = list(A = NULL))
  y <- array(1:6, c(2L, 3L), dimnames = list(NULL, B = NULL))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(A = NULL, B = NULL)
  )
})

# ------------------------------------------------------------------------------
# rray_broadcast_names_common()

test_that("rray_broadcast_names_common() errors on names that disagree", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_snapshot(error = TRUE, {
    rray_broadcast_names_common(x, y, .dimensions = c(2L, 3L))
    rray_broadcast_names_common(x = x, y = y, .dimensions = c(2L, 3L))
  })
})

test_that("rray_broadcast_names_common() reports the input that set the names", {
  x <- array(1:2, c(1L, 2L), dimnames = list("r1", c("a", "b")))
  y <- array(1:2, c(1L, 2L))
  z <- array(1:2, c(1L, 2L), dimnames = list("r1", c("a", "c")))
  expect_snapshot(
    error = TRUE,
    rray_broadcast_names_common(x, y, z, .dimensions = c(1L, 2L))
  )

  x <- array(1:2, c(2L, 1L), dimnames = list(c("a", "b"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list(NULL, c("c", "d")))
  z <- array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), c("c", "e")))
  expect_snapshot(
    error = TRUE,
    rray_broadcast_names_common(x, y, z, .dimensions = c(2L, 2L))
  )
})

test_that("rray_broadcast_names_common() reports the input that set the title", {
  x <- array(1:2, 2L, dimnames = list(A = NULL))
  y <- array(1:2, 2L)
  z <- array(1:2, 2L, dimnames = list(B = NULL))
  expect_snapshot(error = TRUE, {
    rray_broadcast_names_common(x, y, z, .dimensions = 2L)
    rray_broadcast_names_common(x = x, y = y, z = z, .dimensions = 2L)
  })
})

test_that("rray_broadcast_names_common() takes each title from a different input", {
  x <- array(1:2, c(2L, 1L), dimnames = list(A = NULL, NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list(NULL, B = NULL))
  expect_identical(
    rray_broadcast_names_common(x, y, .dimensions = c(2L, 2L)),
    list(A = NULL, B = NULL)
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
