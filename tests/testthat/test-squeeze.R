test_that("squeezes selected axes", {
  x <- array(1:10, c(10L, 1L, 1L))

  expect_identical(rray_dimensions(rray_squeeze(x, 2L)), c(10L, 1L))
  expect_identical(rray_dimensions(rray_squeeze(x, c(2L, 3L))), 10L)
  expect_identical(as.vector(rray_squeeze(x, c(2L, 3L))), 1:10)
})

test_that("squeezing no axes returns the input unchanged", {
  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_squeeze(x, integer()), x)

  out <- rray_squeeze(1:3, integer())
  expect_identical(rray_dimensions(out), 3L)
  expect_identical(as.vector(out), 1:3)
})

test_that("squeezing every axis returns a one-dimensional array", {
  x <- array(1L, c(1L, 1L, 1L))
  out <- rray_squeeze(x, c(1L, 2L, 3L))

  expect_identical(rray_dimensions(out), 1L)
  expect_identical(as.vector(out), 1L)
})

test_that("works with every native type", {
  xs <- list(
    array(TRUE, c(1L, 1L)),
    array(1L, c(1L, 1L)),
    array(1, c(1L, 1L)),
    array(1i, c(1L, 1L)),
    array("x", c(1L, 1L)),
    array(as.raw(1), c(1L, 1L)),
    array(list("x"), c(1L, 1L))
  )

  for (x in xs) {
    out <- rray_squeeze(x, 2L)
    expect_identical(typeof(out), typeof(x))
    expect_identical(out[[1]], x[[1]])
    expect_identical(rray_dimensions(out), 1L)
  }
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 1L, 1L))
  expect_identical(rray_dimensions(rray_squeeze(x, c(2L, 3L))), 0L)

  x <- array(integer(), c(0L, 0L, 1L))
  expect_identical(rray_dimensions(rray_squeeze(x, 3L)), c(0L, 0L))

  x <- array(integer(), c(0L, 0L))
  expect_identical(rray_squeeze(x, integer()), x)
})

test_that("surviving axes keep their names", {
  x <- array(
    1:6,
    c(1L, 2L, 1L, 3L),
    dimnames = list("drop1", c("a", "b"), "drop3", c("x", "y", "z"))
  )
  out <- rray_squeeze(x, c(1L, 3L))

  expect_identical(rray_dimensions(out), c(2L, 3L))
  expect_identical(dimnames(out), list(c("a", "b"), c("x", "y", "z")))
})

test_that("surviving unnamed axes remain unnamed", {
  x <- array(
    1:6,
    c(1L, 2L, 1L, 3L),
    dimnames = list("drop1", c("a", "b"), "drop3", NULL)
  )
  out <- rray_squeeze(x, c(1L, 3L))

  expect_identical(dimnames(out), list(c("a", "b"), NULL))
})

test_that("squeezed axes drop their names", {
  x <- array(1:5, c(1L, 5L), dimnames = list("drop", letters[1:5]))
  expect_identical(dimnames(rray_squeeze(x, 1L)), list(letters[1:5]))

  x <- array(1L, c(1L, 1L), dimnames = list("drop1", "drop2"))
  expect_null(dimnames(rray_squeeze(x, c(1L, 2L))))

  x <- array(1L, 1L, dimnames = list("drop"))
  expect_null(dimnames(rray_squeeze(x, 1L)))

  x <- array(1:2, c(1L, 2L), dimnames = list("drop", NULL))
  expect_null(dimnames(rray_squeeze(x, 1L)))

  x <- c(drop = 1L)
  out <- rray_squeeze(x, 1L)
  expect_identical(rray_dimensions(out), 1L)
  expect_null(dimnames(out))
})

test_that("does not modify the input", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  rray_squeeze(x, 2L)

  expect_identical(dim(x), c(1L, 1L))
  expect_identical(dimnames(x), list("a", "b"))
})

test_that("axes are coerced to integer", {
  x <- array(1:3, c(3L, 1L))
  expect_identical(rray_squeeze(x, 2), rray_squeeze(x, 2L))
})

test_that("errors when a selected axis does not have dimension 1", {
  expect_snapshot(error = TRUE, {
    rray_squeeze(array(1:2, c(2L, 1L)), 1L)
    rray_squeeze(array(integer(), c(0L, 1L)), 1L)
  })
})

test_that("errors on invalid axes", {
  x <- array(1L, c(1L, 1L))

  expect_snapshot(error = TRUE, {
    rray_squeeze(x, 3L)
    rray_squeeze(x, 0L)
    rray_squeeze(x, NA_integer_)
    rray_squeeze(x, c(1L, 1L))
    rray_squeeze(x, c(2L, 1L))
    rray_squeeze(x, structure(1L, foo = "bar"))
    rray_squeeze(x, 1.5)
    rray_squeeze(x, "x")
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_squeeze(NULL, 1L)
    rray_squeeze(mean, 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_squeeze(x, 1L), error = TRUE)
})
