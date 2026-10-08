# ------------------------------------------------------------------------------
# rray_rep()

test_that("`rray_rep()` repeats the axis, `rray_rep_each()` repeats slices", {
  x <- array(1:3, 3)

  expect_identical(
    rray_rep(x, times = 2, axes = 1),
    array(c(1L, 2L, 3L, 1L, 2L, 3L), 6)
  )
  expect_identical(
    rray_rep_each(x, times = 2, axis = 1),
    array(c(1L, 1L, 2L, 2L, 3L, 3L), 6)
  )
})

test_that("repeats along every axis", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_rep(x, times = 2, axes = 1),
    x[c(1:2, 1:2), , , drop = FALSE]
  )
  expect_identical(
    rray_rep(x, times = 2, axes = 2),
    x[, c(1:3, 1:3), , drop = FALSE]
  )
  expect_identical(
    rray_rep(x, times = 2, axes = 3),
    x[,, c(1:4, 1:4), drop = FALSE]
  )
})

test_that("repeats several axes at once", {
  x <- matrix(1:4, nrow = 2)

  expect_identical(
    rray_rep(x, times = 2, axes = c(1, 2)),
    x[c(1:2, 1:2), c(1:2, 1:2)]
  )
  expect_identical(
    rray_rep(x, times = c(2, 3), axes = c(1, 2)),
    x[c(1:2, 1:2), c(1:2, 1:2, 1:2)]
  )
})

test_that("`times[[i]]` is used for `axes[[i]]`", {
  x <- array(1:24, c(2, 3, 4))

  out <- rray_rep(x, times = c(3, 2), axes = c(1, 3))

  expect_identical(dim(out), c(6L, 3L, 8L))
  expect_identical(out, x[rep.int(1:2, 3), , rep.int(1:4, 2), drop = FALSE])
})

test_that("repeating several axes is repeating them one at a time", {
  x <- array(1:24, c(2, 3, 4))

  out <- rray_rep(x, times = c(2, 3), axes = c(1, 3))

  expect_identical(
    out,
    rray_rep(rray_rep(x, times = 2, axes = 1), times = 3, axes = 3)
  )
  expect_identical(
    out,
    rray_rep(rray_rep(x, times = 3, axes = 3), times = 2, axes = 1)
  )
})

test_that("is a slice of the repeated axis with repeated locations", {
  x <- array(
    1:6,
    c(3, 2),
    dimnames = list(rows = c("a", "b", "c"), cols = c("x", "y"))
  )

  expect_identical(
    rray_rep(x, times = 2, axes = 1),
    rray_slice_axis(x, rep.int(1:3, 2), axis = 1)
  )
  expect_identical(
    rray_rep(x, times = 3, axes = 2),
    rray_slice_axis(x, rep.int(1:2, 3), axis = 2)
  )
})

test_that("skips axes with `times = 1`", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_rep(x, times = c(1, 2, 1), axes = c(1, 2, 3)),
    rray_rep(x, times = 2, axes = 2)
  )
  expect_identical(
    rray_rep(x, times = c(2, 1, 3), axes = c(1, 2, 3)),
    rray_rep(x, times = c(2, 3), axes = c(1, 3))
  )
})

test_that("`times = 0` on one axis gives a zero dimension, others still repeat", {
  x <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b")))

  out <- rray_rep(x, times = c(2, 0), axes = c(1, 2))
  expect_identical(dim(out), c(4L, 0L))
  expect_identical(rray_names(out), list(c("r1", "r2", "r1", "r2"), NULL))

  out <- rray_rep(x, times = c(0, 2), axes = c(1, 2))
  expect_identical(dim(out), c(0L, 4L))
  expect_identical(rray_names(out), list(NULL, c("a", "b", "a", "b")))
})

test_that("works with bare vectors", {
  expect_identical(rray_rep(1:3, times = 2, axes = 1), array(c(1:3, 1:3), 6))
})

test_that("works with 1D arrays", {
  expect_identical(
    rray_rep(array(1:3), times = 2, axes = 1),
    array(c(1:3, 1:3), 6)
  )
})

test_that("works with every type", {
  inputs <- list(
    c(TRUE, NA, FALSE, TRUE, NA, FALSE),
    c(1L, NA, 3L, 4L, NA, 6L),
    c(1.5, NA, 3.5, 4.5, NA, 6.5),
    c(1 + 1i, NA, 3 + 3i, 4 + 4i, NA, 6 + 6i),
    as.raw(1:6),
    c("a", NA, "c", "d", NA, "f"),
    list(1, "a", NULL, 2, "b", NULL)
  )

  for (input in inputs) {
    x <- array(input, c(3, 2))
    expect_identical(
      rray_rep(x, times = 2, axes = 1),
      expected_rep(x, 2, 1)
    )
    expect_identical(
      rray_rep(x, times = 2, axes = 2),
      expected_rep(x, 2, 2)
    )
    expect_identical(
      rray_rep(x, times = c(2, 3), axes = c(1, 2)),
      expected_rep(x, c(2, 3), c(1, 2))
    )
  }
})

test_that("`times = 0` gives a zero dimension", {
  x <- array(1:6, c(3, 2))

  expect_identical(rray_rep(x, times = 0, axes = 1), array(integer(), c(0, 2)))
  expect_identical(rray_rep(x, times = 0, axes = 2), array(integer(), c(3, 0)))
})

test_that("`times = 1` returns the input", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  expect_identical(rray_rep(x, times = 1, axes = 1), x)
  expect_identical(rray_rep(x, times = 1, axes = 2), x)
  expect_identical(rray_rep(x, times = 1, axes = c(1, 2)), x)
})

test_that("empty `axes` returns the input", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  expect_identical(rray_rep(x, times = 2, axes = integer()), x)
  expect_identical(rray_rep(x, times = integer(), axes = integer()), x)
})

test_that("works with a zero dimension axis", {
  x <- array(integer(), c(0, 2))
  expect_identical(rray_rep(x, times = 3, axes = 1), x)
  expect_identical(rray_rep(x, times = 3, axes = 2), array(integer(), c(0, 6)))
})

test_that("does no work for a zero dimension on an axis not in `axes`", {
  x <- array(integer(), c(0, 100000, 100000))

  out <- rray_rep(x, times = 2, axes = c(2, 3))

  expect_identical(dim(out), c(0L, 200000L, 200000L))
})

test_that("`times` and `axes` can be doubles", {
  x <- array(1:6, c(3, 2))
  expect_identical(
    rray_rep(x, times = c(2, 3), axes = c(1, 2)),
    rray_rep(x, times = c(2L, 3L), axes = c(1L, 2L))
  )
})

test_that("names on the repeated axes repeat, and can be duplicated", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))

  expect_identical(
    dimnames(rray_rep(x, times = 2, axes = 1)),
    list(c("a", "b", "c", "a", "b", "c"), c("x", "y"))
  )
  expect_identical(
    dimnames(rray_rep(x, times = 2, axes = 2)),
    list(c("a", "b", "c"), c("x", "y", "x", "y"))
  )
  expect_identical(
    dimnames(rray_rep(x, times = c(2, 3), axes = c(1, 2))),
    list(c("a", "b", "c", "a", "b", "c"), c("x", "y", "x", "y", "x", "y"))
  )
})

test_that("partial and missing dimension names are handled", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), NULL))

  expect_identical(
    dimnames(rray_rep(x, times = 2, axes = 2)),
    list(c("a", "b", "c"), NULL)
  )
  expect_identical(
    dimnames(rray_rep(x, times = 2, axes = c(1, 2))),
    list(c("a", "b", "c", "a", "b", "c"), NULL)
  )

  expect_null(dimnames(rray_rep(array(1:6, c(3, 2)), times = 2, axes = 1)))
  expect_null(
    dimnames(rray_rep(array(1:6, c(3, 2)), times = 2, axes = c(1, 2)))
  )
})

test_that("`x` is not modified", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  before <- x

  out <- rray_rep(x, times = 2, axes = c(1, 2))
  out[[1]] <- 100L
  dimnames(out) <- NULL

  expect_identical(x, before)
})

test_that("matches a reference implementation", {
  shapes <- list(5L, c(3L, 2L), c(2L, 3L), c(1L, 6L, 2L), c(2L, 3L, 4L))

  for (dimensions in shapes) {
    x <- array(seq_len(prod(dimensions)), dimensions)

    named <- x
    dimnames(named) <- lapply(dimensions, \(dimension) {
      paste0("n", seq_len(dimension))
    })

    dimensionality <- length(dimensions)

    subsets <- lapply(seq_len(2^dimensionality) - 1L, \(bits) {
      which(bitwAnd(bits, 2^(seq_len(dimensionality) - 1L)) > 0)
    })

    for (axes in subsets) {
      plans <- c(
        as.list(0:3),
        list(seq_along(axes), rev(seq_along(axes)) - 1L)
      )

      for (times in plans) {
        expect_identical(
          rray_rep(x, times = times, axes = axes),
          expected_rep(x, times, axes)
        )
        expect_identical(
          rray_rep(named, times = times, axes = axes),
          expected_rep(named, times, axes)
        )
      }
    }
  }
})

test_that("`times` and `axes` must be named", {
  x <- array(1:6, c(3, 2))

  expect_snapshot(error = TRUE, {
    rray_rep(x, 2, 1)
    rray_rep(x, 2, axes = 1)
    rray_rep(x, times = 2, axis = 1)
  })
})

test_that("`axes` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep(x, times = 2, axes = TRUE), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axes = c(1, 1)), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axes = c(2, 1)), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axes = 0), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axes = 3), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axes = NA), error = TRUE)
})

test_that("`times` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep(x, times = TRUE, axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = NA_integer_, axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = -1, axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = 1.5, axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = Inf, axes = 1), error = TRUE)
  expect_snapshot(
    rray_rep(x, times = .Machine$integer.max + 1, axes = 1),
    error = TRUE
  )
  expect_snapshot(rray_rep(x, times = NaN, axes = 1), error = TRUE)
  expect_snapshot(
    rray_rep(x, times = structure(1L, names = "a"), axes = 1),
    error = TRUE
  )
  expect_snapshot(rray_rep(x, times = c(a = 1), axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = c(1, 2), axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = c(1, 2, 3), axes = c(1, 2)), error = TRUE)
  expect_snapshot(rray_rep(x, times = integer(), axes = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = integer(), axes = c(1, 2)), error = TRUE)
})

test_that("errors if the repeated dimension is too large", {
  x <- array(1:2, 2)
  expect_snapshot(
    rray_rep(x, times = .Machine$integer.max, axes = 1),
    error = TRUE
  )
})

test_that("errors if the repeated size is too large", {
  x <- array(1L, c(1, 1))
  expect_snapshot(rray_rep(x, times = 2^30, axes = c(1, 2)), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_rep(NULL, times = 2, axes = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_rep(x, times = 2, axes = 1), error = TRUE)
})

# ------------------------------------------------------------------------------
# rray_rep_each()

test_that("repeats along every axis", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_rep_each(x, times = 2, axis = 1),
    x[rep(1:2, each = 2), , , drop = FALSE]
  )
  expect_identical(
    rray_rep_each(x, times = 2, axis = 2),
    x[, rep(1:3, each = 2), , drop = FALSE]
  )
  expect_identical(
    rray_rep_each(x, times = 2, axis = 3),
    x[,, rep(1:4, each = 2), drop = FALSE]
  )
})

test_that("`times` is recycled to the dimension of `axis`", {
  x <- array(1:6, c(3, 2))
  expect_identical(
    rray_rep_each(x, times = 2, axis = 1),
    rray_rep_each(x, times = c(2, 2, 2), axis = 1)
  )
})

test_that("`times` can vary by slice", {
  x <- array(1:6, c(3, 2))
  expect_identical(
    rray_rep_each(x, times = c(1, 2, 3), axis = 1),
    x[c(1, 2, 2, 3, 3, 3), , drop = FALSE]
  )
})

test_that("`times` can vary by slice along a middle axis", {
  x <- array(1:24, c(2, 3, 4))
  expect_identical(
    rray_rep_each(x, times = c(1, 2, 3), axis = 2),
    x[, c(1, 2, 2, 3, 3, 3), , drop = FALSE]
  )
  expect_identical(
    rray_rep_each(x, times = c(2, 0, 1), axis = 2),
    x[, c(1, 1, 3), , drop = FALSE]
  )
})

test_that("zeroes in `times` drop individual slices", {
  x <- array(1:3, 3)
  expect_identical(
    rray_rep_each(x, times = c(1, 0, 2), axis = 1),
    array(c(1L, 3L, 3L), 3)
  )
  expect_identical(rray_rep_each(x, times = 0, axis = 1), array(integer(), 0))
})

test_that("is a slice of `axis` with each location repeated", {
  x <- array(
    1:6,
    c(3, 2),
    dimnames = list(rows = c("a", "b", "c"), cols = c("x", "y"))
  )

  expect_identical(
    rray_rep_each(x, times = c(1, 0, 2), axis = 1),
    rray_slice_axis(x, c(1, 3, 3), axis = 1)
  )
  expect_identical(
    rray_rep_each(x, times = 2, axis = 2),
    rray_slice_axis(x, c(1, 1, 2, 2), axis = 2)
  )
})

test_that("works with bare vectors", {
  expect_identical(
    rray_rep_each(1:3, times = 2, axis = 1),
    array(c(1L, 1L, 2L, 2L, 3L, 3L), 6)
  )
})

test_that("works with 1D arrays", {
  expect_identical(
    rray_rep_each(array(1:3), times = 2, axis = 1),
    array(c(1L, 1L, 2L, 2L, 3L, 3L), 6)
  )
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
      rray_rep_each(x, times = 2, axis = 1),
      expected_rep_each(x, 2, 1)
    )
    expect_identical(
      rray_rep_each(x, times = 2, axis = 2),
      expected_rep_each(x, 2, 2)
    )
    expect_identical(
      rray_rep_each(x, times = c(1, 0, 2), axis = 1),
      expected_rep_each(x, c(1, 0, 2), 1)
    )
    expect_identical(
      rray_rep_each(x, times = c(2, 3), axis = 2),
      expected_rep_each(x, c(2, 3), 2)
    )
  }
})

test_that("`times = 1` returns the input", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  expect_identical(rray_rep_each(x, times = 1, axis = 1), x)
  expect_identical(rray_rep_each(x, times = 1, axis = 2), x)
})

test_that("works with a zero dimension axis", {
  x <- array(integer(), c(0, 2))
  expect_identical(rray_rep_each(x, times = 3, axis = 1), x)
  expect_identical(rray_rep_each(x, times = integer(), axis = 1), x)
})

test_that("does no work for a zero dimension with many slices", {
  x <- array(integer(), c(0, 100000, 100000))

  out <- rray_rep_each(x, times = 2, axis = 1)
  expect_identical(dim(out), c(0L, 100000L, 100000L))

  out <- rray_rep_each(x, times = 2, axis = 2)
  expect_identical(dim(out), c(0L, 200000L, 100000L))
})

test_that("`times` can be a double", {
  x <- array(1:6, c(3, 2))
  expect_identical(
    rray_rep_each(x, times = c(1, 2, 3), axis = 1),
    rray_rep_each(x, times = c(1L, 2L, 3L), axis = 1)
  )
})

test_that("names on the repeated axis repeat, and can be duplicated", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))

  expect_identical(
    dimnames(rray_rep_each(x, times = 2, axis = 1)),
    list(c("a", "a", "b", "b", "c", "c"), c("x", "y"))
  )
  expect_identical(
    dimnames(rray_rep_each(x, times = c(1, 0, 2), axis = 1)),
    list(c("a", "c", "c"), c("x", "y"))
  )
})

test_that("partial and missing dimension names are handled", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), NULL))
  expect_identical(
    dimnames(rray_rep_each(x, times = 2, axis = 2)),
    list(c("a", "b", "c"), NULL)
  )

  expect_null(dimnames(rray_rep_each(array(1:6, c(3, 2)), times = 2, axis = 1)))
})

test_that("matches a reference implementation", {
  shapes <- list(5L, c(3L, 2L), c(2L, 3L), c(1L, 6L, 2L), c(2L, 3L, 4L))

  for (dimensions in shapes) {
    x <- array(seq_len(prod(dimensions)), dimensions)

    named <- x
    dimnames(named) <- lapply(dimensions, \(dimension) {
      paste0("n", seq_len(dimension))
    })

    for (axis in seq_along(dimensions)) {
      dimension <- dimensions[[axis]]

      plans <- list(
        0L,
        1L,
        2L,
        rep(1L, dimension),
        seq_len(dimension),
        c(0L, rep(2L, dimension - 1L))
      )

      for (times in plans) {
        expect_identical(
          rray_rep_each(x, times = times, axis = axis),
          expected_rep_each(x, times, axis)
        )
        expect_identical(
          rray_rep_each(named, times = times, axis = axis),
          expected_rep_each(named, times, axis)
        )
      }
    }
  }
})

test_that("`times` and `axis` must be named", {
  x <- array(1:6, c(3, 2))

  expect_snapshot(error = TRUE, {
    rray_rep_each(x, 2, 1)
    rray_rep_each(x, 2, axis = 1)
  })
})

test_that("`axis` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep_each(x, times = 2, axis = 0), error = TRUE)
  expect_snapshot(rray_rep_each(x, times = 2, axis = 3), error = TRUE)
})

test_that("`times` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep_each(x, times = NA_integer_, axis = 1), error = TRUE)
  expect_snapshot(rray_rep_each(x, times = c(1, -1, 1), axis = 1), error = TRUE)
  expect_snapshot(rray_rep_each(x, times = c(1, 2), axis = 1), error = TRUE)
  expect_snapshot(rray_rep_each(x, times = integer(), axis = 1), error = TRUE)
})

test_that("errors if the repeated dimension is too large", {
  x <- array(1:2, 2)
  max <- .Machine$integer.max
  expect_snapshot(rray_rep_each(x, times = max, axis = 1), error = TRUE)
  expect_snapshot(rray_rep_each(x, times = c(max, max), axis = 1), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_rep_each(NULL, times = 2, axis = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_rep_each(x, times = 2, axis = 1), error = TRUE)
})
