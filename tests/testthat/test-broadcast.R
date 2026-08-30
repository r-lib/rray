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
