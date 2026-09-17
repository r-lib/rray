test_that("inserts axes at the front, middle, and back", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_insert_axes(x, 1L), array(1:24, c(1L, 2L, 3L, 4L)))
  expect_identical(rray_insert_axes(x, 2L), array(1:24, c(2L, 1L, 3L, 4L)))
  expect_identical(rray_insert_axes(x, 4L), array(1:24, c(2L, 3L, 4L, 1L)))
})

test_that("inserts multiple axes at once", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_insert_axes(x, c(1L, 5L)),
    array(1:24, c(1L, 2L, 3L, 4L, 1L))
  )
  expect_identical(
    rray_insert_axes(x, c(2L, 3L)),
    array(1:24, c(2L, 1L, 1L, 3L, 4L))
  )
  expect_identical(
    rray_insert_axes(array(1:6, 6L), c(1L, 2L, 3L)),
    array(1:6, c(1L, 1L, 1L, 6L))
  )
})

test_that("inserting no axes returns the input unchanged", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("x", "y", "z"))
  )
  expect_identical(rray_insert_axes(x, integer()), x)

  x <- c(a = 1L, b = 2L, c = 3L)
  expected <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  expect_identical(rray_insert_axes(x, integer()), expected)
})

test_that("works with every native type", {
  xs <- list(
    array(TRUE, 1L),
    array(1L, 1L),
    array(1, 1L),
    array(1i, 1L),
    array("x", 1L),
    array(as.raw(1), 1L),
    array(list("x"), 1L)
  )

  for (x in xs) {
    expect_identical(rray_insert_axes(x, 2L), array(x, c(1L, 1L)))
  }
})

test_that("works with zero-size arrays", {
  x <- array(integer(), 0L)
  expect_identical(rray_insert_axes(x, 1L), array(integer(), c(1L, 0L)))

  x <- array(integer(), c(0L, 0L))
  expect_identical(
    rray_insert_axes(x, 2L),
    array(integer(), c(0L, 1L, 0L))
  )
})

test_that("inserts into a bare vector", {
  expect_identical(rray_insert_axes(1:3, 1L), array(1:3, c(1L, 3L)))
  expect_identical(rray_insert_axes(1:3, 2L), array(1:3, c(3L, 1L)))
})

test_that("existing axes carry their names to their new locations", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("x", "y", "z"))
  )
  expected <- array(
    1:6,
    c(1L, 2L, 1L, 3L),
    dimnames = list(NULL, c("a", "b"), NULL, c("x", "y", "z"))
  )

  expect_identical(rray_insert_axes(x, c(1L, 3L)), expected)
})

test_that("unnamed axes remain unnamed", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  expected <- array(
    1:6,
    c(2L, 1L, 3L),
    dimnames = list(c("a", "b"), NULL, NULL)
  )

  expect_identical(rray_insert_axes(x, 2L), expected)

  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_insert_axes(x, 2L), array(1:6, c(2L, 1L, 3L)))
})

test_that("does not modify the input", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  expected <- x
  rray_insert_axes(x, 2L)

  expect_identical(x, expected)
})

test_that("`at` is coerced to integer", {
  x <- array(1:3, 3L)
  expect_identical(rray_insert_axes(x, 2), array(1:3, c(3L, 1L)))
})

test_that("squeezing the inserted axes returns the input", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("x", "y", "z"))
  )
  at <- c(1L, 3L)

  expect_identical(rray_squeeze(rray_insert_axes(x, at), at), x)
})

test_that("errors on invalid `at`", {
  x <- array(1L, c(1L, 1L))

  expect_snapshot(error = TRUE, {
    rray_insert_axes(x, 4L)
    rray_insert_axes(x, 0L)
    rray_insert_axes(x, NA_integer_)
    rray_insert_axes(x, c(1L, 1L))
    rray_insert_axes(x, c(2L, 1L))
    rray_insert_axes(x, structure(1L, foo = "bar"))
    rray_insert_axes(x, 1.5)
    rray_insert_axes(x, "x")
  })
})

test_that("errors on dimensionality upper bound", {
  expect_snapshot(error = TRUE, {
    rray_insert_axes(array(1, dim = rep(1L, 64)), 1L)
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_insert_axes(NULL, 1L)
    rray_insert_axes(mean, 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_insert_axes(x, 1L), error = TRUE)
})
