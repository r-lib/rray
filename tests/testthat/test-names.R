test_that("returns NULL for unnamed plain vectors", {
  expect_null(rray_names(1:3))
  expect_null(rray_names("a"))
})

test_that("returns names for named plain vectors", {
  expect_identical(rray_names(c(a = 1, b = 2)), list(c("a", "b")))
  expect_identical(rray_names(c(x = 1L)), list("x"))
})

test_that("returns dimnames for arrays", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(
    rray_names(x),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("returns NULL for unnamed arrays", {
  expect_null(rray_names(array(1:6, c(2, 3))))
  expect_null(rray_names(array(1:24, c(2, 3, 4))))
})

test_that("handles partially named dimnames", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(rray_names(x), list(c("r1", "r2"), NULL))
})

test_that("errors on non-vector types", {
  expect_snapshot(rray_names(NULL), error = TRUE)
  expect_snapshot(rray_names(mean), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_names(x), error = TRUE)
})

test_that("rray_axis_names() gets names for a single axis", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(rray_axis_names(x, 1), c("r1", "r2"))
  expect_identical(rray_axis_names(x, 2), c("c1", "c2", "c3"))
})

test_that("rray_axis_names() works with 3+ dimensional arrays", {
  x <- array(1:24, c(2, 3, 4))
  expect_null(rray_axis_names(x, 3))

  x <- rray_set_axis_names(x, 3, letters[1:4])
  expect_identical(rray_axis_names(x, 3), letters[1:4])
})

test_that("rray_axis_names() returns NULL for an unnamed axis", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  expect_null(rray_axis_names(x, 2))
})

test_that("rray_axis_names() returns NULL when there are no names at all", {
  expect_null(rray_axis_names(array(1:6, c(2, 3)), 1))
})

test_that("rray_axis_names() coerces axis to integer", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(rray_axis_names(x, 1), rray_axis_names(x, 1L))
})

test_that("rray_axis_names() errors on an invalid axis", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_axis_names(x, 0), error = TRUE)
  expect_snapshot(rray_axis_names(x, 3), error = TRUE)
  expect_snapshot(rray_axis_names(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_axis_names(x, c(1, 2)), error = TRUE)
  expect_snapshot(rray_axis_names(x, integer()), error = TRUE)
})

test_that("rray_axis_names() errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_axis_names(x, 1), error = TRUE)
})

test_that("rray_row_names() and rray_col_names() are axis shortcuts", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(rray_row_names(x), rray_axis_names(x, 1))
  expect_identical(rray_col_names(x), rray_axis_names(x, 2))
})

test_that("rray_col_names() errors if `x` doesn't have a second axis", {
  expect_snapshot(rray_col_names(1:5), error = TRUE)
})

test_that("rray_row_names() and rray_col_names() error on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_row_names(x), error = TRUE)
  expect_snapshot(rray_col_names(x), error = TRUE)
})

test_that("rray_set_names() sets names for every axis", {
  x <- array(1:6, c(2, 3))
  out <- rray_set_names(x, list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(
    dimnames(out),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_set_names() allows some axes to be unnamed", {
  x <- array(1:6, c(2, 3))
  out <- rray_set_names(x, list(c("r1", "r2"), NULL))
  expect_identical(dimnames(out), list(c("r1", "r2"), NULL))
})

test_that("rray_set_names() with `NULL` clears all names", {
  x <- array(
    1:6,
    c(2, 3),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_set_names(x, NULL)
  expect_null(dimnames(out))
})

test_that("rray_set_names() doesn't touch the data", {
  x <- array(1:6, c(2, 3))
  out <- rray_set_names(x, list(c("r1", "r2"), NULL))
  expect_identical(unname(out), x)
})

test_that("rray_set_names() works with zero-size axes", {
  x <- array(integer(), c(0, 3))
  # A zero-length character vector is normalized to `NULL` by R itself
  out <- rray_set_names(x, list(character(), c("c1", "c2", "c3")))
  expect_identical(dimnames(out), list(NULL, c("c1", "c2", "c3")))
})

test_that("rray_set_names() errors if `names` isn't a list or `NULL`", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_names(x, c("r1", "r2")), error = TRUE)
})

test_that("rray_set_names() errors if length doesn't match dimensionality", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_names(x, list(c("r1", "r2"))), error = TRUE)
  expect_snapshot(
    rray_set_names(x, list(c("r1", "r2"), NULL, NULL)),
    error = TRUE
  )
})

test_that("rray_set_names() errors if an axis' names aren't character", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_names(x, list(1:2, NULL)), error = TRUE)
})

test_that("rray_set_names() errors if an axis' names are the wrong length", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_names(x, list("r1", NULL)), error = TRUE)
})

test_that("rray_set_names() errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_set_names(x, NULL), error = TRUE)
})

test_that("rray_set_axis_names() sets names for a single axis", {
  x <- array(1:6, c(2, 3))
  out <- rray_set_axis_names(x, 1, c("r1", "r2"))
  expect_identical(dimnames(out), list(c("r1", "r2"), NULL))
})

test_that("rray_set_axis_names() leaves other axes untouched", {
  x <- array(
    1:6,
    c(2, 3),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_set_axis_names(x, 1, c("a", "b"))
  expect_identical(dimnames(out), list(c("a", "b"), c("c1", "c2", "c3")))
})

test_that("rray_set_axis_names() with `NULL` clears just that axis", {
  x <- array(
    1:6,
    c(2, 3),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_set_axis_names(x, 1, NULL)
  expect_identical(dimnames(out), list(NULL, c("c1", "c2", "c3")))
})

test_that("rray_set_axis_names() coerces axis to integer", {
  x <- array(1:6, c(2, 3))
  expect_identical(
    rray_set_axis_names(x, 1, c("r1", "r2")),
    rray_set_axis_names(x, 1L, c("r1", "r2"))
  )
})

test_that("rray_set_axis_names() errors on an invalid axis", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_axis_names(x, 0, "a"), error = TRUE)
  expect_snapshot(rray_set_axis_names(x, 3, "a"), error = TRUE)
})

test_that("rray_set_axis_names() errors if names aren't character or NULL", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_axis_names(x, 1, 1:2), error = TRUE)
})

test_that("rray_set_axis_names() errors if names are the wrong length", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_axis_names(x, 1, "r1"), error = TRUE)
})

test_that("rray_set_axis_names() errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_set_axis_names(x, 1, NULL), error = TRUE)
})

test_that("rray_set_row_names() and rray_set_col_names() are axis shortcuts", {
  x <- array(1:6, c(2, 3))
  expect_identical(
    rray_set_row_names(x, c("r1", "r2")),
    rray_set_axis_names(x, 1, c("r1", "r2"))
  )
  expect_identical(
    rray_set_col_names(x, c("c1", "c2", "c3")),
    rray_set_axis_names(x, 2, c("c1", "c2", "c3"))
  )
})

test_that("rray_set_row_names() and rray_set_col_names() error on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_set_row_names(x, NULL), error = TRUE)
  expect_snapshot(rray_set_col_names(x, NULL), error = TRUE)
})

test_that("rray_broadcast_names2() prefers `x` where its dimension is kept", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  y <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() falls through to `y` where `x` broadcasts", {
  x <- array(1:3, c(1, 3), dimnames = list("only", c("c1", "c2", "c3")))
  y <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("a", "b"), c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() falls through to `y` on an unnamed axis", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  y <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("d", "e", "g")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("r1", "r2"), c("d", "e", "g"))
  )
})

test_that("rray_broadcast_names2() takes each axis from a different input", {
  x <- array(1:3, c(3, 1), dimnames = list(c("r1", "r2", "r3"), NULL))
  y <- array(1:2, c(1, 2), dimnames = list(NULL, c("c1", "c2")))
  expect_identical(
    rray_broadcast_names2(x, y, c(3L, 2L)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
})

test_that("rray_broadcast_names2() returns NULL when neither input has names", {
  x <- array(1:6, c(2, 3))
  y <- array(1:6, c(2, 3))
  expect_null(rray_broadcast_names2(x, y, c(2L, 3L)))
})

test_that("rray_broadcast_names2() returns NULL when every axis broadcasts", {
  x <- array(1, c(1, 1), dimnames = list("a", "b"))
  y <- array(1, c(1, 1), dimnames = list("c", "d"))
  expect_null(rray_broadcast_names2(x, y, c(2L, 2L)))
})

test_that("rray_broadcast_names2() keeps names of an axis broadcast from 1 to 1", {
  x <- array(1, c(1, 1), dimnames = list("a", "b"))
  y <- array(1:3, c(1, 3))
  expect_identical(rray_broadcast_names2(x, y, c(1L, 3L)), list("a", NULL))
})

test_that("rray_broadcast_names2() handles new trailing axes", {
  x <- c(a = 1, b = 2)
  y <- array(1:6, c(2, 3), dimnames = list(NULL, c("p", "q", "r")))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L)),
    list(c("a", "b"), c("p", "q", "r"))
  )
})

test_that("rray_broadcast_names2() works with 3+ dimensional arrays", {
  x <- array(1:24, c(2, 3, 4))
  x <- rray_set_axis_names(x, 3, letters[1:4])
  y <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(
    rray_broadcast_names2(x, y, c(2L, 3L, 4L)),
    list(c("r1", "r2"), NULL, letters[1:4])
  )
})

test_that("rray_broadcast_names2() works with zero-size axes", {
  x <- array(integer(), c(0, 3), dimnames = list(NULL, c("c1", "c2", "c3")))
  y <- array(integer(), c(0, 3))
  expect_identical(
    rray_broadcast_names2(x, y, c(0L, 3L)),
    list(NULL, c("c1", "c2", "c3"))
  )
})

test_that("rray_broadcast_names2() errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_broadcast_names2(x, 1L, c(2L, 2L)), error = TRUE)
  expect_snapshot(rray_broadcast_names2(1L, x, c(2L, 2L)), error = TRUE)
})

test_that("rray_broadcast_names2() errors on non-vector input", {
  expect_snapshot(rray_broadcast_names2(NULL, 1L, 1L), error = TRUE)
  expect_snapshot(rray_broadcast_names2(1L, NULL, 1L), error = TRUE)
})

test_that("rray_broadcast_names2() errors on invalid `dimensions`", {
  x <- c(a = 1, b = 2)
  expect_snapshot(rray_broadcast_names2(x, 1L, "a"), error = TRUE)
  expect_snapshot(rray_broadcast_names2(x, 1L, integer()), error = TRUE)
  expect_snapshot(rray_broadcast_names2(x, 1L, NA_integer_), error = TRUE)
})
