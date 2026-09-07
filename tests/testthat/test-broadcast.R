# ------------------------------------------------------------------------------
# rray_broadcast()

test_that("broadcasts dimension of size 1 to size N", {
  x <- array(1L, c(1L, 2L))
  out <- rray_broadcast(x, c(3L, 2L))
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(out, array(1L, c(3L, 2L)))
})

test_that("returns input unchanged when dimensions already match", {
  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_broadcast(x, c(2L, 3L)), x)
})

test_that("can broadcast up to a new dimension", {
  x <- array(1:6, c(2L, 3L))
  out <- rray_broadcast(x, c(2L, 3L, 2L))
  expect_identical(rray_dimensions(out), c(2L, 3L, 2L))
  expect_identical(out[,, 1], x)
  expect_identical(out[,, 2], x)
})

test_that("broadcasts a plain vector", {
  out <- rray_broadcast(1:3, c(3L, 2L))
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(out[, 1], 1:3)
  expect_identical(out[, 2], 1:3)
})

test_that("broadcasts with zero-length dimensions", {
  x <- integer()
  out <- rray_broadcast(x, c(0L, 2L))
  expect_identical(rray_dimensions(out), c(0L, 2L))
  expect_identical(rray_size(out), 0)
})

test_that("can broadcast 0 to 0 but not 0 to N", {
  x <- array(integer(), c(0L, 2L))
  expect_identical(
    rray_broadcast(x, c(0L, 2L)),
    x
  )
  expect_snapshot(rray_broadcast(x, c(1L, 2L)), error = TRUE)
})

test_that("can't broadcast from N to M when N > 1 and N != M", {
  x <- array(1L, c(2L, 3L))
  expect_snapshot(rray_broadcast(x, c(2L, 4L)), error = TRUE)
})

test_that("can't decrease dimensionality", {
  x <- array(1L, c(2L, 3L, 4L))
  expect_snapshot(rray_broadcast(x, c(2L, 3L)), error = TRUE)
})

test_that("broadcasts logical arrays", {
  x <- array(c(TRUE, FALSE), c(2L, 1L))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(out[, 1], c(TRUE, FALSE))
  expect_identical(out[, 3], c(TRUE, FALSE))
})

test_that("broadcasts integer arrays", {
  x <- array(1:4, c(2L, 2L))
  out <- rray_broadcast(x, c(2L, 2L, 2L))
  expect_identical(out[,, 1], x)
  expect_identical(out[,, 2], x)
})

test_that("broadcasts double arrays", {
  x <- array(c(1.5, 2.5), c(1L, 2L))
  out <- rray_broadcast(x, c(3L, 2L))
  expect_identical(out[1, ], c(1.5, 2.5))
  expect_identical(out[3, ], c(1.5, 2.5))
})

test_that("broadcasts complex arrays", {
  x <- array(c(1 + 2i, 3 + 4i), c(2L, 1L))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(out[, 1], c(1 + 2i, 3 + 4i))
  expect_identical(out[, 3], c(1 + 2i, 3 + 4i))
})

test_that("broadcasts character arrays", {
  x <- array(c("a", "b"), c(2L, 1L))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(out[, 1], c("a", "b"))
  expect_identical(out[, 3], c("a", "b"))
})

test_that("broadcasts raw arrays", {
  x <- array(as.raw(1:2), c(2L, 1L))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(out[, 1], as.raw(1:2))
  expect_identical(out[, 3], as.raw(1:2))
})

test_that("broadcasts list arrays", {
  x <- array(list("a", 1L), c(2L, 1L))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(out[, 1], list("a", 1L))
  expect_identical(out[, 3], list("a", 1L))
})

test_that("preserves dimension names when sizes match", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(dimnames(out), list(c("r1", "r2"), c("c1", "c2", "c3")))
})

test_that("drops dimension names when a dimension is broadcast", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("r1", "r2"), "c1"))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(dimnames(out), list(c("r1", "r2"), NULL))
})

test_that("drops all dimension names when all dimensions are broadcast", {
  x <- array(1L, c(1L, 1L), dimnames = list("r1", "c1"))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_null(dimnames(out))
})

test_that("drops all dimension names when all dimension names were `NULL`", {
  x <- array(1L, c(1L, 1L), dimnames = list(NULL, NULL))

  # When we actually perform any broadcasting, the `dimnames` are cleared
  out <- rray_broadcast(x, c(2L, 1L))
  expect_null(dimnames(out))

  # If we no-op due to same dimensions, they aren't cleared.
  # We accept this irregularity in favor of performance, since it is fairly
  # common to want to broadcast to no-op common dimensions.
  out <- rray_broadcast(x, c(1L, 1L))
  expect_identical(dimnames(out), list(NULL, NULL))
})

test_that("preserves names when broadcasting a named vector", {
  x <- c(a = 1L, b = 2L, c = 3L)
  out <- rray_broadcast(x, c(3L, 2L))
  expect_identical(dimnames(out), list(c("a", "b", "c"), NULL))
})

test_that("no dimnames when broadcasting an unnamed vector", {
  out <- rray_broadcast(1:3, c(3L, 2L))
  expect_null(dimnames(out))
})

test_that("new dimensions get NULL names", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  out <- rray_broadcast(x, c(2L, 3L, 2L))
  expect_identical(
    dimnames(out),
    list(c("r1", "r2"), c("c1", "c2", "c3"), NULL)
  )
})

test_that("handles partially named dimnames during broadcast", {
  x <- array(1:6, c(2L, 3L), dimnames = list(NULL, c("c1", "c2", "c3")))
  out <- rray_broadcast(x, c(2L, 3L))
  expect_identical(dimnames(out), list(NULL, c("c1", "c2", "c3")))
})

test_that("dimension titles are lost", {
  x <- array(1L, c(1L, 1L), dimnames = list(rows = "r1", cols = "c1"))

  # When we actually perform any broadcasting, the titles are cleared
  out <- rray_broadcast(x, c(2L, 1L))
  expect_identical(rray_names(out), list(NULL, "c1"))

  out <- rray_broadcast(x, c(2L, 2L))
  expect_null(rray_names(out))

  # If we no-op due to same dimensions, they aren't cleared.
  # We accept this irregularity in favor of performance, since it is fairly
  # common to want to broadcast to no-op common dimensions.
  out <- rray_broadcast(x, c(1L, 1L))
  expect_identical(
    rray_names(out),
    list(rows = "r1", cols = "c1")
  )
})

test_that("errors on non-array input", {
  expect_snapshot(rray_broadcast(NULL, 1L), error = TRUE)
  expect_snapshot(rray_broadcast(mean, 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_broadcast(x, c(2L, 2L)), error = TRUE)
})

test_that("coerces dimensions to integer", {
  out <- rray_broadcast(1, c(2, 3))
  expect_identical(rray_dimensions(out), c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_broadcast(1, 2.5)
  })
})

test_that("errors on non-coercible dimensions", {
  expect_snapshot(rray_broadcast(1, "a"), error = TRUE)
})

test_that("errors on empty dimensions", {
  expect_snapshot(rray_broadcast(1, integer()), error = TRUE)
})

test_that("errors on missing dimensions", {
  expect_snapshot(rray_broadcast(1, NA_integer_), error = TRUE)
})

test_that("errors on negative dimensions", {
  expect_snapshot(rray_broadcast(1, -1L), error = TRUE)
})

test_that("errors on dimensionality upper bound", {
  expect_snapshot(error = TRUE, {
    rray_broadcast(array(1, dim = rep(1L, 64)), rep(1L, 65))
  })
})

test_that("broadcasting carries through singleton axes for each storage type", {
  inputs <- list(
    c(TRUE, FALSE),
    1:4,
    as.double(1:4),
    1:4 + 1i,
    as.raw(1:4),
    letters[1:4],
    as.list(1:4)
  )

  for (input in inputs) {
    x <- array(input, c(1L, 2L, 1L, 2L))
    expected <- x[rep(1L, 2L), , rep(1L, 3L), , drop = FALSE]
    expect_identical(rray_broadcast(x, c(2L, 2L, 3L, 2L)), expected)
  }
})

# ------------------------------------------------------------------------------
# rray_broadcast_common()

test_that("broadcasts every input to the common dimensions", {
  x <- array(1:3, c(3L, 1L))
  y <- array(1:2, c(1L, 2L))
  out <- rray_broadcast_common(x, y)
  expect_identical(rray_dimensions(out[[1]]), c(3L, 2L))
  expect_identical(rray_dimensions(out[[2]]), c(3L, 2L))
  expect_identical(out[[1]][, 1], 1:3)
  expect_identical(out[[2]][1, ], 1:2)
})

test_that("dimensionality is extended", {
  out <- rray_broadcast_common(1:2, array(1L, c(2L, 3L, 4L)))
  expect_identical(rray_dimensions(out[[1]]), c(2L, 3L, 4L))
  expect_identical(rray_dimensions(out[[2]]), c(2L, 3L, 4L))
})

test_that("leaves inputs alone when nothing changes", {
  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_broadcast_common(x, x), list(x, x))
})

test_that("single input works", {
  expect_identical(rray_broadcast_common(1:3), list(array(1:3)))
})

test_that("works with zero size dimensions", {
  out <- rray_broadcast_common(
    array(integer(), c(0L, 1L)),
    array(1L, c(1L, 2L))
  )
  expect_identical(rray_dimensions(out[[1]]), c(0L, 2L))
  expect_identical(rray_dimensions(out[[2]]), c(0L, 2L))
})

test_that("doesn't touch types", {
  out <- rray_broadcast_common(1L, 2.5, "a", TRUE, 1i, as.raw(1), list(1))
  expect_identical(
    vapply(out, typeof, character(1)),
    c("integer", "double", "character", "logical", "complex", "raw", "list")
  )
})

test_that("keeps the names of `...`", {
  out <- rray_broadcast_common(x = 1:3, y = 1L)
  expect_named(out, c("x", "y"))

  out <- rray_broadcast_common(1:3, y = 1L)
  expect_named(out, c("", "y"))

  expect_null(names(rray_broadcast_common(1:3, 1L)))
})

test_that("follows the dimension names rule", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("r1", "r2"), "c1"))
  out <- rray_broadcast_common(x, array(1L, c(2L, 3L)))
  expect_identical(dimnames(out[[1]]), list(c("r1", "r2"), NULL))
})

test_that("`.dimensions` overrides the common dimensions", {
  out <- rray_broadcast_common(1:3, .dimensions = c(3L, 2L))
  expect_identical(rray_dimensions(out[[1]]), c(3L, 2L))
})

test_that("errors on `NULL` input", {
  expect_snapshot(rray_broadcast_common(NULL, 1:2), error = TRUE)
  expect_snapshot(rray_broadcast_common(NULL), error = TRUE)
})

test_that("errors on classed input in `...`", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_broadcast_common(y = x), error = TRUE)
})

test_that("errors name the input that failed", {
  expect_snapshot(error = TRUE, {
    rray_broadcast_common(x = 1:3, y = 1:2)
  })
  expect_snapshot(error = TRUE, {
    rray_broadcast_common(a = 1:2, b = 1:3, .dimensions = c(2L, 2L))
  })
  expect_snapshot(error = TRUE, {
    rray_broadcast_common(1:2, array(1L, c(2L, 2L)), .dimensions = 2L)
  })
})
