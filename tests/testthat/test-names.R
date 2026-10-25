# ------------------------------------------------------------------------------
# rray_names()

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

# ------------------------------------------------------------------------------
# rray_axis_names()

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

test_that("rray_axis_names() takes a title for axis", {
  x <- array(
    1:6,
    c(2, 3),
    dimnames = list(row = NULL, col = c("c1", "c2", "c3"))
  )
  expect_identical(rray_axis_names(x, "col"), c("c1", "c2", "c3"))
  expect_null(rray_axis_names(x, "row"))
})

test_that("rray_axis_names() errors on an invalid axis", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_axis_names(x, 0), error = TRUE)
  expect_snapshot(rray_axis_names(x, 3), error = TRUE)
  expect_snapshot(rray_axis_names(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_axis_names(x, c(1, 2)), error = TRUE)
  expect_snapshot(rray_axis_names(x, integer()), error = TRUE)
  expect_snapshot(rray_axis_names(x, "row"), error = TRUE)
})

test_that("rray_axis_names() errors on an unknown title", {
  x <- array(1:6, c(2, 3), dimnames = list(row = NULL, col = NULL))
  expect_snapshot(rray_axis_names(x, "nope"), error = TRUE)
})

test_that("rray_axis_names() errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_axis_names(x, 1), error = TRUE)
})

test_that("rray_row_names() and rray_column_names() are axis shortcuts", {
  x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_identical(rray_row_names(x), rray_axis_names(x, 1))
  expect_identical(rray_column_names(x), rray_axis_names(x, 2))
})

test_that("rray_column_names() errors if `x` doesn't have a second axis", {
  expect_snapshot(rray_column_names(1:5), error = TRUE)
})

test_that("rray_row_names() and rray_column_names() error on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_row_names(x), error = TRUE)
  expect_snapshot(rray_column_names(x), error = TRUE)
})

# ------------------------------------------------------------------------------
# rray_set_names()

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

test_that("rray_set_names() takes titles from `names`", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(A = c("a", "b"), B = NULL, C = NULL)
  )
  y <- array(1:24, c(2L, 3L, 4L), dimnames = list(A = NULL, B = NULL, C = NULL))

  out <- rray_set_names(x, list(NULL, NULL, NULL))
  expect_null(rray_titles(out))

  out <- rray_set_names(x, list(D = NULL, E = NULL, F = NULL))
  expect_identical(dimnames(out), list(D = NULL, E = NULL, F = NULL))

  out <- rray_set_names(y, list(c("a", "b"), NULL, NULL))
  expect_identical(dimnames(out), list(c("a", "b"), NULL, NULL))

  expect_null(dimnames(rray_set_names(y, NULL)))
})

test_that("rray_set_names() doesn't touch the data", {
  x <- array(1:6, c(2, 3))
  out <- rray_set_names(x, list(c("r1", "r2"), NULL))
  expect_identical(unname(out), x)
})

test_that("rray_set_names() drops other attributes", {
  x <- array(1:4, c(2L, 2L))
  attr(x, "foo") <- 1
  names(x) <- letters[1:4]

  names <- list(c("a", "b"), c("c", "d"))
  expect_identical(
    rray_set_names(x, names),
    array(1:4, c(2L, 2L), dimnames = names)
  )
  expect_identical(rray_set_names(x, NULL), array(1:4, c(2L, 2L)))
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

# ------------------------------------------------------------------------------
# rray_set_axis_names()

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

test_that("rray_set_axis_names() keeps titles", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(A = c("a", "b"), B = NULL, C = NULL)
  )
  y <- array(1:24, c(2L, 3L, 4L), dimnames = list(A = NULL, B = NULL, C = NULL))

  out <- rray_set_axis_names(x, 2, c("c", "d", "e"))
  expect_identical(
    dimnames(out),
    list(A = c("a", "b"), B = c("c", "d", "e"), C = NULL)
  )

  out <- rray_set_axis_names(x, 1, NULL)
  expect_identical(dimnames(out), list(A = NULL, B = NULL, C = NULL))

  out <- rray_set_axis_names(y, 3, c("f", "g", "h", "i"))
  expect_identical(
    dimnames(out),
    list(A = NULL, B = NULL, C = c("f", "g", "h", "i"))
  )
})

test_that("rray_set_axis_names() doesn't modify `x`", {
  x <- array(1:6, c(2L, 3L), dimnames = list(A = c("a", "b"), B = NULL))
  rray_set_axis_names(x, 1, c("c", "d"))
  expect_identical(dimnames(x), list(A = c("a", "b"), B = NULL))
})

test_that("rray_set_axis_names() drops other attributes", {
  x <- array(1:4, c(2L, 2L))
  attr(x, "foo") <- 1
  names(x) <- letters[1:4]
  expect_identical(
    rray_set_axis_names(x, 1L, c("a", "b")),
    array(1:4, c(2L, 2L), dimnames = list(c("a", "b"), NULL))
  )
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

test_that("rray_set_axis_names() takes a title for axis", {
  x <- array(1:6, c(2, 3), dimnames = list(row = NULL, col = NULL))
  expect_identical(
    rray_set_axis_names(x, "col", c("c1", "c2", "c3")),
    rray_set_axis_names(x, 2, c("c1", "c2", "c3"))
  )
})

test_that("rray_set_axis_names() errors on an invalid axis", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_set_axis_names(x, 0, "a"), error = TRUE)
  expect_snapshot(rray_set_axis_names(x, 3, "a"), error = TRUE)
  expect_snapshot(rray_set_axis_names(x, "row", "a"), error = TRUE)
})

test_that("rray_set_axis_names() errors on an unknown title", {
  x <- array(1:6, c(2, 3), dimnames = list(row = NULL, col = NULL))
  expect_snapshot(rray_set_axis_names(x, "nope", "a"), error = TRUE)
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

test_that("rray_set_row_names() and rray_set_column_names() are axis shortcuts", {
  x <- array(1:6, c(2, 3))
  expect_identical(
    rray_set_row_names(x, c("r1", "r2")),
    rray_set_axis_names(x, 1, c("r1", "r2"))
  )
  expect_identical(
    rray_set_column_names(x, c("c1", "c2", "c3")),
    rray_set_axis_names(x, 2, c("c1", "c2", "c3"))
  )
})

test_that("rray_set_row_names() and rray_set_column_names() keep titles", {
  x <- array(1:6, c(2L, 3L), dimnames = list(A = NULL, B = NULL))

  out <- rray_set_row_names(x, c("a", "b"))
  expect_identical(dimnames(out), list(A = c("a", "b"), B = NULL))

  out <- rray_set_column_names(x, c("c", "d", "e"))
  expect_identical(dimnames(out), list(A = NULL, B = c("c", "d", "e")))
})

test_that("names work with long vectors", {
  skip_if_not_testing_long_vectors()

  x <- array(as.raw(0), c(2^16, 2^15))
  x[length(x)] <- as.raw(1)

  rows <- paste0("r", seq_len(2^16))
  columns <- paste0("c", seq_len(2^15))

  out <- rray_set_names(x, list(rows, columns))
  expect_identical(rray_names(out), list(rows, columns))
  expect_identical(out[length(out)], as.raw(1))

  out <- rray_set_axis_names(x, 2, columns)
  expect_identical(rray_axis_names(out, 2), columns)
  expect_identical(out[length(out)], as.raw(1))

  out <- rray_set_row_names(x, rows)
  expect_identical(rray_row_names(out), rows)
  expect_identical(out[length(out)], as.raw(1))

  out <- rray_set_column_names(x, columns)
  expect_identical(rray_column_names(out), columns)
  expect_identical(out[length(out)], as.raw(1))
})

test_that("rray_set_row_names() and rray_set_column_names() error on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_set_row_names(x, NULL), error = TRUE)
  expect_snapshot(rray_set_column_names(x, NULL), error = TRUE)
})
