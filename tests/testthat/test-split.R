test_that("splits along every axis with a uniform size", {
  x <- array(1:24, c(4, 3, 2))

  out <- rray_split(x, 1, 1)
  expect_length(out, 4)
  expect_identical(out[[1]], array(x[1, , ], c(1, 3, 2)))
  expect_identical(out[[4]], array(x[4, , ], c(1, 3, 2)))

  out <- rray_split(x, 2, 1)
  expect_length(out, 3)
  expect_identical(out[[1]], array(x[, 1, ], c(4, 1, 2)))
  expect_identical(out[[3]], array(x[, 3, ], c(4, 1, 2)))

  out <- rray_split(x, 3, 1)
  expect_length(out, 2)
  expect_identical(out[[1]], array(1:12, c(4, 3, 1)))
  expect_identical(out[[2]], array(13:24, c(4, 3, 1)))
})

test_that("a uniform size can be larger than 1", {
  x <- array(1:12, c(6, 2))

  out <- rray_split(x, 1, 2)
  expect_length(out, 3)
  expect_identical(out[[1]], array(c(1L, 2L, 7L, 8L), c(2, 2)))
  expect_identical(out[[3]], array(c(5L, 6L, 11L, 12L), c(2, 2)))

  out <- rray_split(x, 1, 6)
  expect_identical(out, list(x))
})

test_that("explicit dimensions give each array directly", {
  x <- array(1:12, c(6, 2))

  out <- rray_split(x, 1, c(3, 3))
  expect_identical(out, rray_split(x, 1, 3))

  out <- rray_split(x, 1, c(1, 5))
  expect_identical(dim(out[[1]]), c(1L, 2L))
  expect_identical(dim(out[[2]]), c(5L, 2L))
  expect_identical(out[[1]], array(c(1L, 7L), c(1, 2)))
})

test_that("explicit dimensions can contain zeroes", {
  x <- array(1:6, c(3, 2))
  empty <- array(integer(), c(0, 2))

  expect_identical(rray_split(x, 1, c(0, 3))[[1]], empty)
  expect_identical(rray_split(x, 1, c(3, 0))[[2]], empty)

  out <- rray_split(x, 1, c(1, 0, 2))
  expect_identical(out[[1]], array(c(1L, 4L), c(1, 2)))
  expect_identical(out[[2]], empty)
  expect_identical(out[[3]], array(c(2L, 3L, 5L, 6L), c(2, 2)))
})

test_that("works with a zero dimension axis", {
  x <- array(integer(), c(0, 2))

  expect_identical(rray_split(x, 1, 1), list())
  expect_identical(rray_split(x, 1, 2), list())
  expect_identical(rray_split(x, 1, integer()), list())
  expect_identical(rray_split(x, 1, c(0, 0)), list(x, x))

  expect_identical(
    rray_split(x, 2, 1),
    list(array(integer(), c(0, 1)), array(integer(), c(0, 1)))
  )
})

test_that("works with 1D arrays", {
  x <- array(1:5)

  out <- rray_split(x, 1, 1)
  expect_length(out, 5)
  expect_identical(out[[1]], array(1L))
  expect_identical(out[[5]], array(5L))

  expect_identical(rray_split(x, 1, c(2, 3)), list(array(1:2), array(3:5)))
})

test_that("works with bare vectors", {
  expect_identical(rray_split(1:4, 1, 2), list(array(1:2), array(3:4)))
})

test_that("works with every type", {
  inputs <- list(
    c(TRUE, NA, FALSE, TRUE),
    c(1L, NA, 3L, 4L),
    c(1.5, NA, 3.5, 4.5),
    c(1 + 1i, NA, 3 + 3i, 4 + 4i),
    as.raw(1:4),
    c("a", NA, "c", "d"),
    list(1, "a", NULL, TRUE)
  )

  for (input in inputs) {
    x <- array(input, c(2, 2))

    expect_identical(rray_split(x, 1, 1), expected_split(x, 1, 1))
    expect_identical(rray_split(x, 2, 1), expected_split(x, 2, 1))
    expect_identical(rray_split(x, 1, c(0, 2)), expected_split(x, 1, c(0, 2)))
  }
})

test_that("matches a reference implementation", {
  shapes <- list(5L, c(6L, 2L), c(2L, 6L), c(1L, 6L, 2L), c(2L, 3L, 4L))

  for (x_dimensions in shapes) {
    x <- array(seq_len(prod(x_dimensions)), x_dimensions)

    named <- x
    dimnames(named) <- lapply(x_dimensions, function(dimension) {
      paste0("n", seq_len(dimension))
    })

    for (axis in seq_along(x_dimensions)) {
      axis_dimension <- x_dimensions[[axis]]

      uniform <- Filter(
        \(dimension) axis_dimension %% dimension == 0L,
        seq_len(axis_dimension)
      )

      plans <- c(
        as.list(uniform),
        list(
          rep(1L, axis_dimension),
          c(0L, axis_dimension),
          c(axis_dimension, 0L),
          c(1L, 0L, axis_dimension - 1L)
        )
      )

      for (dimensions in plans) {
        expect_identical(
          rray_split(x, axis, dimensions),
          expected_split(x, axis, dimensions)
        )
        expect_identical(
          rray_split(named, axis, dimensions),
          expected_split(named, axis, dimensions)
        )
      }
    }
  }
})

test_that("names on non-split axes are kept", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 2, 1)
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), "x"))
  expect_identical(dimnames(out[[3]]), list(c("a", "b"), "z"))
})

test_that("names on the split axis are sliced", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 2, c(2, 1))
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), c("x", "y")))
  expect_identical(dimnames(out[[2]]), list(c("a", "b"), "z"))

  out <- rray_split(x, 1, 1)
  expect_identical(dimnames(out[[1]]), list("a", c("x", "y", "z")))
  expect_identical(dimnames(out[[2]]), list("b", c("x", "y", "z")))
})

test_that("zero size arrays drop the names on the split axis", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 2, c(0, 3))
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), NULL))
  expect_identical(dimnames(out[[2]]), list(c("a", "b"), c("x", "y", "z")))
})

test_that("partial dimension names are handled", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), NULL))

  out <- rray_split(x, 1, 1)
  expect_identical(dimnames(out[[1]]), list("a", NULL))

  out <- rray_split(x, 2, 1)
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), NULL))
})

test_that("NULL dimension names are handled", {
  x <- array(1:6, c(2, 3))
  expect_null(dimnames(rray_split(x, 1, 1)[[1]]))

  x <- array(1:6, c(2, 3), dimnames = list(NULL, NULL))
  expect_null(dimnames(rray_split(x, 1, 1)[[1]]))
})

test_that("the result list is unnamed", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))
  expect_null(names(rray_split(x, 1, 1)))
  expect_null(names(rray_split(x, 2, c(1, 2))))
})

test_that("`x` is not modified", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))
  before <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 2, 1)
  expect_identical(x, before)

  out[[1]][[1]] <- 100L
  dimnames(out[[1]]) <- list(c("A", "B"), "X")
  expect_identical(x, before)
})

test_that("combining the arrays reproduces the input", {
  x <- array(
    1:24,
    c(2, 3, 4),
    dimnames = list(c("a", "b"), c("x", "y", "z"), NULL)
  )

  for (axis in seq_along(dim(x))) {
    axis_dimension <- dim(x)[[axis]]

    plans <- list(
      1L,
      axis_dimension,
      c(1L, axis_dimension - 1L),
      c(0L, axis_dimension)
    )

    for (dimensions in plans) {
      arrays <- rray_split(x, axis, dimensions)
      expect_identical(rray_combine(!!!arrays, .axis = axis), x)
    }
  }
})

test_that("`axis` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_split(x, 0, 1), error = TRUE)
  expect_snapshot(rray_split(x, 3, 1), error = TRUE)
  expect_snapshot(rray_split(x, c(1, 2), 1), error = TRUE)
  expect_snapshot(rray_split(x, NA_integer_, 1), error = TRUE)
  expect_snapshot(rray_split(x, 1.5, 1), error = TRUE)
  expect_snapshot(rray_split(x, structure(1L, class = "foo"), 1), error = TRUE)
})

test_that("`dimensions` are validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_split(x, 1), error = TRUE)
  expect_snapshot(rray_split(x, 1, NA_integer_), error = TRUE)
  expect_snapshot(rray_split(x, 1, 1.5), error = TRUE)
  expect_snapshot(rray_split(x, 1, structure(1L, names = "a")), error = TRUE)
  expect_snapshot(rray_split(x, 1, 0), error = TRUE)
  expect_snapshot(rray_split(x, 1, -1), error = TRUE)
  expect_snapshot(rray_split(x, 2, 2), error = TRUE)
  expect_snapshot(rray_split(x, 1, c(1, -1)), error = TRUE)
  expect_snapshot(rray_split(x, 1, c(1, 2)), error = TRUE)
  expect_snapshot(rray_split(x, 2, c(1, 1)), error = TRUE)
  expect_snapshot(rray_split(x, 1, integer()), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_split(NULL, 1, 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_split(x, 1, 1), error = TRUE)
})
