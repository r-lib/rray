test_that("unstacks a 2D array along every axis", {
  x <- array(1:6, c(2, 3))

  out <- rray_unstack(x, 1)
  expect_length(out, 2)
  expect_identical(out[[1]], array(c(1L, 3L, 5L), 3))
  expect_identical(out[[2]], array(c(2L, 4L, 6L), 3))

  out <- rray_unstack(x, 2)
  expect_length(out, 3)
  expect_identical(out[[1]], array(1:2, 2))
  expect_identical(out[[3]], array(5:6, 2))
})

test_that("unstacks a 3D array along every axis", {
  x <- array(1:24, c(2, 3, 4))

  for (axis in 1:3) {
    out <- rray_unstack(x, axis)
    expect_length(out, dim(x)[[axis]])

    for (i in seq_along(out)) {
      expect_identical(out[[i]], stack_slice(x, axis, i))
    }
  }
})

test_that("works with every type", {
  xs <- list(
    lgl = c(TRUE, FALSE),
    int = 1:2,
    dbl = c(1, 2),
    cpl = c(1i, 2i),
    chr = c("a", "b"),
    raw = as.raw(1:2),
    list = list(1, "x")
  )

  for (x in xs) {
    expect_identical(rray_unstack(array(x, c(2, 1)), 2), list(array(x, 2)))
  }
})

test_that("works with a zero dimension axis", {
  x <- array(integer(), c(0, 2))

  expect_identical(rray_unstack(x, 1), list())
  expect_identical(
    rray_unstack(x, 2),
    list(array(integer(), 0), array(integer(), 0))
  )
})

test_that("works with an axis of dimension 1", {
  x <- array(1:3, c(1, 3))
  expect_identical(rray_unstack(x, 1), list(array(1:3, 3)))
})

test_that("names on the axis become the list names", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_unstack(x, 1)
  expect_named(out, c("a", "b"))
  expect_identical(
    out$a,
    array(c(1L, 3L, 5L), 3, dimnames = list(c("x", "y", "z")))
  )

  out <- rray_unstack(x, 2)
  expect_named(out, c("x", "y", "z"))
  expect_identical(out$x, array(1:2, 2, dimnames = list(c("a", "b"))))
})

test_that("names on the surviving axes move to their new locations", {
  x <- array(
    1:24,
    c(2, 3, 4),
    dimnames = list(c("a", "b"), c("x", "y", "z"), NULL)
  )

  out <- rray_unstack(x, 1)
  expect_named(out, c("a", "b"))
  expect_identical(dimnames(out[[1]]), list(c("x", "y", "z"), NULL))

  out <- rray_unstack(x, 2)
  expect_named(out, c("x", "y", "z"))
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), NULL))

  out <- rray_unstack(x, 3)
  expect_null(names(out))
  expect_identical(dimnames(out[[1]]), list(c("a", "b"), c("x", "y", "z")))
})

test_that("partial dimension names are handled", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), NULL))

  out <- rray_unstack(x, 1)
  expect_named(out, c("a", "b"))
  expect_null(dimnames(out[[1]]))

  out <- rray_unstack(x, 2)
  expect_null(names(out))
  expect_identical(dimnames(out[[1]]), list(c("a", "b")))
})

test_that("`x` is not modified", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))
  before <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_unstack(x, 2)
  expect_identical(x, before)

  out[[1]][[1]] <- 100L
  dimnames(out[[1]]) <- list(c("A", "B"))
  expect_identical(x, before)
})

test_that("stacking the arrays reproduces the input", {
  xs <- list(
    array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z"))),
    array(1:24, c(2, 3, 4), dimnames = list(c("a", "b"), NULL, letters[1:4]))
  )

  for (x in xs) {
    for (axis in seq_along(dim(x))) {
      arrays <- rray_unstack(x, axis)
      expect_identical(rray_stack(!!!arrays, .axis = axis), x)
    }
  }
})

test_that("unstacking a stack reproduces the inputs", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))
  y <- x + 6L

  for (axis in 1:3) {
    out <- rray_stack(p = x, q = y, .axis = axis)
    expect_identical(rray_unstack(out, axis), list(p = x, q = y))
  }
})

test_that("unstacking a stack normalizes broadcast and cast inputs", {
  x <- array(1:2, c(2, 1))
  y <- array(1:6, c(2, 3))

  expect_identical(
    rray_unstack(rray_stack(x, y, .axis = 1), 1),
    list(rray_broadcast(x, c(2, 3)), y)
  )

  expect_identical(
    rray_unstack(rray_stack(1L, 2, .axis = 1), 1),
    list(array(1), array(2))
  )
})

test_that("`axis` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_unstack(x, 0), error = TRUE)
  expect_snapshot(rray_unstack(x, 3), error = TRUE)
  expect_snapshot(rray_unstack(x, c(1, 2)), error = TRUE)
  expect_snapshot(rray_unstack(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_unstack(x, 1.5), error = TRUE)
  expect_snapshot(rray_unstack(x, structure(1L, class = "foo")), error = TRUE)
})

test_that("1D arrays are rejected", {
  expect_snapshot(rray_unstack(array(1:3), 1), error = TRUE)
  expect_snapshot(rray_unstack(1:3, 1), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_unstack(NULL, 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_unstack(x, 1), error = TRUE)
})
