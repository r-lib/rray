# ------------------------------------------------------------------------------
# rray_roll()

test_that("positive shifts move toward the end, negative toward the start", {
  x <- array(1:5, 5)

  expect_identical(rray_roll(x, n = 2, axes = 1), array(c(4:5, 1:3), 5))
  expect_identical(rray_roll(x, n = -1, axes = 1), array(c(2:5, 1L), 5))
})

test_that("shifts wrap around the dimension", {
  x <- array(1:5, 5)

  expect_identical(rray_roll(x, n = 7, axes = 1), array(c(4:5, 1:3), 5))
  expect_identical(rray_roll(x, n = -6, axes = 1), array(c(2:5, 1L), 5))
})

test_that("rolls along every axis", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_roll(x, n = 1, axes = 1),
    rray_slice_axis(x, c(2, 1), axis = 1)
  )
  expect_identical(
    rray_roll(x, n = 1, axes = 2),
    rray_slice_axis(x, c(3, 1, 2), axis = 2)
  )
  expect_identical(
    rray_roll(x, n = 1, axes = 3),
    rray_slice_axis(x, c(4, 1, 2, 3), axis = 3)
  )
})

test_that("rolls several axes at once", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

  expect <- rray_slice(x, c(2, 1), c(3, 1, 2))

  expect_identical(rray_roll(x, n = 1, axes = c(1, 2)), expect)
  expect_identical(rray_roll(x, n = c(1, 1), axes = c(1, 2)), expect)
})

test_that("`n[[i]]` is the shift for `axes[[i]]`", {
  x <- array(1:24, c(2, 3, 4))

  out <- rray_roll(x, n = c(-1, 1), axes = c(1, 3))

  expect_identical(out, rray_slice(x, c(2, 1), TRUE, c(4, 1, 2, 3)))
  expect_identical(
    rray_slice_axis(out, 1, axis = 3),
    rray_slice(x, c(2, 1), TRUE, 4)
  )
})

test_that("rolling several axes is rolling them one at a time", {
  x <- array(1:60, c(3, 4, 5))

  expect <- rray_roll(x, n = c(1, 2), axes = c(1, 3))

  expect_identical(
    rray_roll(rray_roll(x, n = 1, axes = 1), n = 2, axes = 3),
    expect
  )
  expect_identical(
    rray_roll(rray_roll(x, n = 2, axes = 3), n = 1, axes = 1),
    expect
  )
})

test_that("rolling by `n` then `-n` gives back `x`", {
  x <- array(1:60, c(3, 4, 5))
  dimnames(x) <- list(letters[1:3], letters[4:7], letters[8:12])

  for (n in c(-7L, -1L, 0L, 1L, 2L, 9L)) {
    for (axis in 1:3) {
      out <- rray_roll(x, n = n, axes = axis)
      expect_identical(rray_roll(out, n = -n, axes = axis), x)
    }
  }
})

test_that("is a slice of the rolled axis with rotated locations", {
  x <- array(1:60, c(3, 4, 5))
  dimnames(x) <- list(letters[1:3], NULL, letters[8:12])

  for (axis in 1:3) {
    d <- dim(x)[[axis]]

    for (k in seq_len(d - 1L)) {
      expect_identical(
        rray_roll(x, n = k, axes = axis),
        rray_slice_axis(x, c((d - k + 1):d, seq_len(d - k)), axis = axis)
      )
    }
  }
})

test_that("works with every type", {
  inputs <- list(
    c(TRUE, NA, FALSE),
    c(1L, NA, 3L),
    c(1.5, NA, 3.5),
    c(1 + 1i, NA, 3 + 3i),
    as.raw(1:3),
    c("a", NA, "c"),
    list(1, "a", NULL)
  )

  for (input in inputs) {
    x <- array(input, c(3, 2))
    expect_identical(
      rray_roll(x, n = 1, axes = 1),
      rray_slice(x, c(3, 1, 2), TRUE)
    )
    expect_identical(
      rray_roll(x, n = 1, axes = 2),
      rray_slice(x, TRUE, c(2, 1))
    )
    expect_identical(
      rray_roll(x, n = c(2, 1), axes = c(1, 2)),
      rray_slice(x, c(2, 3, 1), c(2, 1))
    )
  }
})

test_that("works with bare vectors", {
  expect_identical(rray_roll(1:5, n = 2, axes = 1), array(c(4:5, 1:3), 5))

  expect_identical(
    rray_roll(c(a = 1L, b = 2L, c = 3L, d = 4L), n = 1, axes = 1),
    array(c(4L, 1:3), 4, dimnames = list(c("d", "a", "b", "c")))
  )
})

test_that("names roll on rolled axes and are untouched elsewhere", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

  expect_identical(
    dimnames(rray_roll(x, n = 1, axes = 2)),
    list(c("r1", "r2"), c("c", "a", "b"))
  )
  expect_identical(
    dimnames(rray_roll(x, n = 1, axes = 1)),
    list(c("r2", "r1"), c("a", "b", "c"))
  )
  expect_identical(
    dimnames(rray_roll(x, n = 1, axes = c(1, 2))),
    list(c("r2", "r1"), c("c", "a", "b"))
  )
})

test_that("partial and missing dimension names are handled", {
  x <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))

  expect_identical(
    dimnames(rray_roll(x, n = 1, axes = 1)),
    list(NULL, c("a", "b", "c"))
  )
  expect_identical(
    dimnames(rray_roll(x, n = 1, axes = 2)),
    list(NULL, c("c", "a", "b"))
  )

  expect_null(dimnames(rray_roll(matrix(1:6, nrow = 2), n = 1, axes = 1)))
})

test_that("returns `x` when nothing moves", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

  expect_identical(rray_roll(x, n = 0, axes = 2), x)
  expect_identical(rray_roll(x, n = 3, axes = 2), x)
  expect_identical(rray_roll(x, n = -6, axes = 2), x)
  expect_identical(rray_roll(x, n = 1, axes = integer()), x)
})

test_that("works with a zero dimension on the rolled axis", {
  x <- array(integer(), c(2, 0))
  expect_identical(rray_roll(x, n = 1, axes = 2), x)
  expect_identical(rray_roll(x, n = -1, axes = c(1, 2)), x)

  x <- array(integer(), 0)
  expect_identical(rray_roll(x, n = 5, axes = 1), x)
})

test_that("names still roll with a zero dimension on another axis", {
  x <- array(integer(), c(3, 0), dimnames = list(c("a", "b", "c"), NULL))

  expect_identical(
    rray_roll(x, n = 1, axes = 1),
    array(integer(), c(3, 0), dimnames = list(c("c", "a", "b"), NULL))
  )
})

test_that("`n` and `axes` can be integerish doubles", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_roll(x, n = c(1, 2), axes = c(2, 3)),
    rray_roll(x, n = c(1L, 2L), axes = c(2L, 3L))
  )
})

test_that("`n` can be empty when `axes` is empty", {
  x <- array(1:6, c(2, 3))
  expect_identical(rray_roll(x, n = integer(), axes = integer()), x)
})

test_that("handles the largest shifts", {
  x <- array(1:5, 5)

  max <- .Machine$integer.max

  expect_identical(
    rray_roll(x, n = max, axes = 1),
    rray_slice(x, c(4, 5, 1, 2, 3))
  )
  expect_identical(
    rray_roll(x, n = -max, axes = 1),
    rray_slice(x, c(3, 4, 5, 1, 2))
  )
})

test_that("`n` and `axes` must be named", {
  x <- array(1:6, c(2, 3))

  expect_snapshot(error = TRUE, {
    rray_roll(x, 1, 2)
    rray_roll(x, 1, axes = 2)
  })
})

test_that("`axes` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_roll(x, n = 1, axes = TRUE), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1, axes = c(1, 1)), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1, axes = c(2, 1)), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1, axes = 3), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1, axes = 0), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1, axes = NA), error = TRUE)
})

test_that("`n` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_roll(x, n = TRUE, axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = c(1, 2, 3), axes = c(1, 2)), error = TRUE)
  expect_snapshot(rray_roll(x, n = c(1, 2), axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = integer(), axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = NA, axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = c(1L, NA), axes = c(1, 2)), error = TRUE)
  expect_snapshot(rray_roll(x, n = 1.5, axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = "a", axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = c(a = 1L), axes = 1), error = TRUE)
  expect_snapshot(rray_roll(x, n = matrix(1L), axes = 1), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_roll(NULL, n = 1, axes = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_roll(x, n = 1, axes = 1), error = TRUE)
})

# ------------------------------------------------------------------------------
# rray_roll_each()

test_that("rolls each row by its own `n`", {
  x <- matrix(1:12, nrow = 3, byrow = TRUE)

  expect_identical(
    rray_roll_each(x, n = c(1, 0, -1), axis = 2),
    matrix(c(4L, 1:3, 5:8, 10:12, 9L), nrow = 3, byrow = TRUE)
  )
})

test_that("rolls each column by its own `n` given as a one row matrix", {
  x <- matrix(1:12, nrow = 3)

  expect_identical(
    rray_roll_each(x, n = matrix(c(0, 1, 2, 3), nrow = 1), axis = 1),
    matrix(c(1:3, 6L, 4:5, 8:9, 7L, 10:12), nrow = 3)
  )
})

test_that("rolls a three dimensional array by one `n` for everything", {
  x <- array(1:30, c(2, 5, 3))

  out <- rray_roll_each(x, n = 1, axis = 2)

  expect_identical(out, rray_slice_axis(x, c(5, 1:4), axis = 2))
})

test_that("rolls a three dimensional array by one `n` per row", {
  x <- array(1:30, c(2, 5, 3))

  out <- rray_roll_each(x, n = c(1, 2), axis = 2)

  expect_identical(
    rray_slice_axis(out, 1, axis = 1),
    rray_slice(x, 1, c(5, 1:4), TRUE)
  )
  expect_identical(
    rray_slice_axis(out, 2, axis = 1),
    rray_slice(x, 2, c(4:5, 1:3), TRUE)
  )
})

test_that("rolls a three dimensional array by one `n` per sheet", {
  x <- array(1:30, c(2, 5, 3))

  out <- rray_roll_each(x, n = array(c(0, 1, 2), c(1, 1, 3)), axis = 2)

  expect_identical(
    rray_slice_axis(out, 1, axis = 3),
    rray_slice_axis(x, 1, axis = 3)
  )
  expect_identical(
    rray_slice_axis(out, 2, axis = 3),
    rray_slice(x, TRUE, c(5, 1:4), 2)
  )
  expect_identical(
    rray_slice_axis(out, 3, axis = 3),
    rray_slice(x, TRUE, c(4:5, 1:3), 3)
  )
})

test_that("rolls a three dimensional array by one `n` per row and sheet", {
  x <- array(1:30, c(2, 5, 3))
  n <- array(1:6, c(2, 1, 3))

  out <- rray_roll_each(x, n = n, axis = 2)

  expect_identical(out, base_roll_each(x, n, axis = 2))
  expect_identical(
    rray_slice_axis(out, 3, axis = 3),
    array(c(21L, 30L, 23L, 22L, 25L, 24L, 27L, 26L, 29L, 28L), c(2, 5, 1))
  )
})

test_that("matches the expected roll along every axis", {
  x <- array(1:60, c(3, 4, 5))

  ns <- list(
    list(
      -4L,
      array(c(0L, -1L, 5L, 2L), c(1, 4)),
      array(c(2L, -3L, 7L, 1L, 0L), c(1, 1, 5)),
      array(-10:9, c(1, 4, 5))
    ),
    list(
      3L,
      c(1L, 0L, -2L),
      array(c(2L, -3L, 7L, 1L, 0L), c(1, 1, 5)),
      array(-7:7, c(3, 1, 5))
    ),
    list(
      -1L,
      c(-7L, 2L, 0L),
      array(1:4, c(1, 4)),
      array(-6:5, c(3, 4))
    )
  )

  for (axis in 1:3) {
    for (n in ns[[axis]]) {
      expect_identical(
        rray_roll_each(x, n = n, axis = axis),
        base_roll_each(x, n, axis)
      )
    }
  }
})

test_that("a single `n` gives the same values as `rray_roll()`", {
  x <- array(1:60, c(3, 4, 5))
  dimnames(x) <- list(letters[1:3], letters[4:7], letters[8:12])

  for (n in c(-7L, -1L, 0L, 1L, 2L, 9L)) {
    for (axis in 1:3) {
      expect_identical(
        rray_roll_each(x, n = n, axis = axis),
        rray_set_axis_names(rray_roll(x, n = n, axes = axis), axis, NULL)
      )
    }
  }
})

test_that("rolling each by `n` then `-n` gives back `x` without `axis` names", {
  x <- array(1:60, c(3, 4, 5))
  dimnames(x) <- list(letters[1:3], letters[4:7], letters[8:12])

  n <- array(-7:7, c(3, 1, 5))

  out <- rray_roll_each(x, n = n, axis = 2)
  out <- rray_roll_each(out, n = -n, axis = 2)

  expect_identical(out, rray_set_axis_names(x, 2, NULL))
})

test_that("rolls every type", {
  inputs <- list(
    c(TRUE, NA, FALSE, TRUE, FALSE, NA),
    c(1L, NA, 3L, 4L, 5L, 6L),
    c(1.5, NA, 3.5, 4.5, 5.5, 6.5),
    c(1 + 1i, NA, 3 + 3i, 4 + 4i, 5 + 5i, 6 + 6i),
    as.raw(1:6),
    c("a", NA, "c", "d", "e", "f"),
    list(1, "a", NULL, 4, "e", NULL)
  )

  for (input in inputs) {
    x <- array(input, c(2, 3))
    expect_identical(
      rray_roll_each(x, n = c(1, 2), axis = 2),
      base_roll_each(x, c(1, 2), axis = 2)
    )
    expect_identical(
      rray_roll_each(x, n = matrix(c(1, 0, 1), nrow = 1), axis = 1),
      base_roll_each(x, matrix(c(1, 0, 1), nrow = 1), axis = 1)
    )
  }
})

test_that("works with a bare vector `x` and a bare vector `n`", {
  expect_identical(
    rray_roll_each(1:5, n = 2, axis = 1),
    array(c(4:5, 1:3), 5)
  )
  expect_identical(
    rray_roll_each(c(a = 1L, b = 2L, c = 3L), n = c(z = 1L), axis = 1),
    array(c(3L, 1:2), 3)
  )
})

test_that("drops names on `axis` and keeps the rest", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

  expect_identical(
    rray_roll_each(x, n = c(1, 2), axis = 2),
    matrix(
      c(5L, 4L, 1L, 6L, 3L, 2L),
      nrow = 2,
      dimnames = list(c("r1", "r2"), NULL)
    )
  )
  expect_identical(
    rray_roll_each(x, n = 1, axis = 2),
    matrix(c(5:6, 1:4), nrow = 2, dimnames = list(c("r1", "r2"), NULL))
  )
  expect_identical(
    rray_roll_each(x, n = matrix(c(1, 0, 1), nrow = 1), axis = 1),
    matrix(
      c(2L, 1L, 3:4, 6:5),
      nrow = 2,
      dimnames = list(NULL, c("a", "b", "c"))
    )
  )
})

test_that("has no names when only `axis` had names", {
  x <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
  expect_null(dimnames(rray_roll_each(x, n = 1, axis = 2)))
})

test_that("ignores names on `n`", {
  x <- matrix(1:6, nrow = 2)

  n <- matrix(1:3, nrow = 1, dimnames = list("r", c("a", "b", "c")))
  expect_identical(
    rray_roll_each(x, n = n, axis = 1),
    rray_roll_each(x, n = unname(n), axis = 1)
  )

  n <- c(a = 1L, b = 2L)
  expect_identical(
    rray_roll_each(x, n = n, axis = 2),
    rray_roll_each(x, n = unname(n), axis = 2)
  )
})

test_that("`n` can be integerish doubles or logicals", {
  x <- matrix(1:12, nrow = 3)

  expect_identical(
    rray_roll_each(x, n = matrix(c(0, 1, 2, 3), nrow = 1), axis = 1),
    rray_roll_each(x, n = matrix(0:3, nrow = 1), axis = 1)
  )
  expect_identical(
    rray_roll_each(x, n = c(TRUE, FALSE, TRUE), axis = 2),
    rray_roll_each(x, n = c(1L, 0L, 1L), axis = 2)
  )
})

test_that("works with a zero dimension", {
  x <- array(integer(), c(2, 0))
  expect_identical(rray_roll_each(x, n = c(1, 2), axis = 2), x)
  expect_identical(rray_roll_each(x, n = 1, axis = 1), x)

  x <- array(integer(), c(0, 3))
  expect_identical(rray_roll_each(x, n = integer(), axis = 2), x)

  x <- array(integer(), 0)
  expect_identical(rray_roll_each(x, n = 5, axis = 1), x)
})

test_that("drops names on `axis` with a zero dimension on another axis", {
  x <- array(integer(), c(3, 0), dimnames = list(c("a", "b", "c"), NULL))
  expect_identical(
    rray_roll_each(x, n = 1, axis = 1),
    array(integer(), c(3, 0))
  )
})

test_that("handles the largest `n`", {
  x <- array(1:5, 5)

  max <- .Machine$integer.max

  expect_identical(
    rray_roll_each(x, n = max, axis = 1),
    rray_slice(x, c(4, 5, 1, 2, 3))
  )
  expect_identical(
    rray_roll_each(x, n = -max, axis = 1),
    rray_slice(x, c(3, 4, 5, 1, 2))
  )
})

test_that("does not modify `x` or `n`", {
  x <- array(1:24, c(2, 3, 4))
  x_copy <- x + 0L

  n <- array(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), c(2, 1, 4))
  n_copy <- n + 0L

  rray_roll_each(x, n = n, axis = 2)

  expect_identical(x, x_copy)
  expect_identical(n, n_copy)
})

test_that("`n` and `axis` must be named", {
  x <- array(1:6, c(2, 3))

  expect_snapshot(error = TRUE, {
    rray_roll_each(x, 1, 2)
    rray_roll_each(x, 1, axis = 2)
  })
})

test_that("`axis` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_roll_each(x, n = 1, axis = c(1, 2)), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = 1, axis = 3), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = 1, axis = 0), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = 1, axis = NA_integer_), error = TRUE)
})

test_that("`n` must broadcast to `x` with `axis` set to 1", {
  x <- matrix(1:12, nrow = 3)
  expect_snapshot(rray_roll_each(x, n = c(0, 1, 2, 3), axis = 1), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = c(1, 2), axis = 2), error = TRUE)
  expect_snapshot(
    rray_roll_each(x, n = array(1L, c(1, 1, 2)), axis = 1),
    error = TRUE
  )

  x <- array(integer(), c(2, 0))
  expect_snapshot(rray_roll_each(x, n = c(1, 2, 3), axis = 2), error = TRUE)
})

test_that("`n` must be an integer array with no missing values", {
  x <- matrix(1:12, nrow = 3)
  expect_snapshot(rray_roll_each(x, n = NA, axis = 1), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = c(1L, NA, 3L), axis = 2), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = 1.5, axis = 1), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = "a", axis = 1), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = factor("a"), axis = 1), error = TRUE)
  expect_snapshot(rray_roll_each(x, n = NULL, axis = 1), error = TRUE)
})

test_that("`x` must be a bare array", {
  expect_snapshot(rray_roll_each(NULL, n = 1, axis = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_roll_each(x, n = 1, axis = 1), error = TRUE)
})
