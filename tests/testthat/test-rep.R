# ------------------------------------------------------------------------------
# rray_rep()

test_that("`rray_rep()` repeats the axis, `rray_rep_each()` repeats slices", {
  x <- array(1:3, 3)

  expect_identical(
    rray_rep(x, times = 2, axis = 1),
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
    rray_rep(x, times = 2, axis = 1),
    x[c(1:2, 1:2), , , drop = FALSE]
  )
  expect_identical(
    rray_rep(x, times = 2, axis = 2),
    x[, c(1:3, 1:3), , drop = FALSE]
  )
  expect_identical(
    rray_rep(x, times = 2, axis = 3),
    x[,, c(1:4, 1:4), drop = FALSE]
  )
})

test_that("works with bare vectors", {
  expect_identical(rray_rep(1:3, times = 2, axis = 1), array(c(1:3, 1:3), 6))
})

test_that("works with 1D arrays", {
  expect_identical(
    rray_rep(array(1:3), times = 2, axis = 1),
    array(c(1:3, 1:3), 6)
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
      rray_rep(x, times = 2, axis = 1),
      expected_rep(x, 2, 1, FALSE)
    )
    expect_identical(
      rray_rep(x, times = 2, axis = 2),
      expected_rep(x, 2, 2, FALSE)
    )
  }
})

test_that("`times = 0` gives a zero dimension", {
  x <- array(1:6, c(3, 2))

  expect_identical(rray_rep(x, times = 0, axis = 1), array(integer(), c(0, 2)))
  expect_identical(rray_rep(x, times = 0, axis = 2), array(integer(), c(3, 0)))
})

test_that("`times = 1` returns the input", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  expect_identical(rray_rep(x, times = 1, axis = 1), x)
  expect_identical(rray_rep(x, times = 1, axis = 2), x)
})

test_that("works with a zero dimension axis", {
  x <- array(integer(), c(0, 2))
  expect_identical(rray_rep(x, times = 3, axis = 1), x)
})

test_that("`times` can be a double", {
  x <- array(1:6, c(3, 2))
  expect_identical(
    rray_rep(x, times = 2, axis = 1),
    rray_rep(x, times = 2L, axis = 1)
  )
})

test_that("names on the repeated axis repeat, and can be duplicated", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))

  expect_identical(
    dimnames(rray_rep(x, times = 2, axis = 1)),
    list(c("a", "b", "c", "a", "b", "c"), c("x", "y"))
  )
  expect_identical(
    dimnames(rray_rep(x, times = 2, axis = 2)),
    list(c("a", "b", "c"), c("x", "y", "x", "y"))
  )
})

test_that("partial and missing dimension names are handled", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), NULL))
  expect_identical(
    dimnames(rray_rep(x, times = 2, axis = 2)),
    list(c("a", "b", "c"), NULL)
  )

  expect_null(dimnames(rray_rep(array(1:6, c(3, 2)), times = 2, axis = 1)))
})

test_that("`x` is not modified", {
  x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
  before <- x

  out <- rray_rep(x, times = 2, axis = 1)
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

    for (axis in seq_along(dimensions)) {
      for (times in 0:3) {
        expect_identical(
          rray_rep(x, times = times, axis = axis),
          expected_rep(x, times, axis, FALSE)
        )
        expect_identical(
          rray_rep(named, times = times, axis = axis),
          expected_rep(named, times, axis, FALSE)
        )
      }
    }
  }
})

test_that("`times` and `axis` must be named", {
  x <- array(1:6, c(3, 2))

  expect_snapshot(error = TRUE, {
    rray_rep(x, 2, 1)
    rray_rep(x, 2, axis = 1)
  })
})

test_that("`axis` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep(x, times = 2, axis = 0), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axis = 3), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axis = c(1, 2)), error = TRUE)
  expect_snapshot(rray_rep(x, times = 2, axis = NA_integer_), error = TRUE)
})

test_that("`times` is validated", {
  x <- array(1:6, c(3, 2))
  expect_snapshot(rray_rep(x, times = NA_integer_, axis = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = -1, axis = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = 1.5, axis = 1), error = TRUE)
  expect_snapshot(
    rray_rep(x, times = structure(1L, names = "a"), axis = 1),
    error = TRUE
  )
  expect_snapshot(rray_rep(x, times = c(1, 2), axis = 1), error = TRUE)
  expect_snapshot(rray_rep(x, times = integer(), axis = 1), error = TRUE)
})

test_that("errors if the repeated dimension is too large", {
  x <- array(1:2, 2)
  expect_snapshot(
    rray_rep(x, times = .Machine$integer.max, axis = 1),
    error = TRUE
  )
})

test_that("errors on invalid input", {
  expect_snapshot(rray_rep(NULL, times = 2, axis = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_rep(x, times = 2, axis = 1), error = TRUE)
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

test_that("zeroes in `times` drop individual slices", {
  x <- array(1:3, 3)
  expect_identical(
    rray_rep_each(x, times = c(1, 0, 2), axis = 1),
    array(c(1L, 3L, 3L), 3)
  )
  expect_identical(rray_rep_each(x, times = 0, axis = 1), array(integer(), 0))
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
      expected_rep(x, 2, 1, TRUE)
    )
    expect_identical(
      rray_rep_each(x, times = 2, axis = 2),
      expected_rep(x, 2, 2, TRUE)
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
          expected_rep(x, times, axis, TRUE)
        )
        expect_identical(
          rray_rep_each(named, times = times, axis = axis),
          expected_rep(named, times, axis, TRUE)
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
  expect_snapshot(rray_rep_each(x, times = c(max, max), axis = 1), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_rep_each(NULL, times = 2, axis = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_rep_each(x, times = 2, axis = 1), error = TRUE)
})
