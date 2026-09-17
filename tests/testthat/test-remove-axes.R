test_that("removes selected axes", {
  x <- array(1:10, c(10L, 1L, 1L))

  expect_identical(rray_remove_axes(x, 2L), array(1:10, c(10L, 1L)))
  expect_identical(rray_remove_axes(x, c(2L, 3L)), array(1:10, 10L))
})

test_that("removing no axes returns the input unchanged", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("x", "y", "z"))
  )
  expect_identical(rray_remove_axes(x, integer()), x)

  x <- c(a = 1L, b = 2L, c = 3L)
  expected <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  expect_identical(rray_remove_axes(x, integer()), expected)
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
    expect_identical(rray_remove_axes(x, 2L), array(x, 1L))
  }
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 1L, 1L))
  expect_identical(rray_remove_axes(x, c(2L, 3L)), array(integer(), 0L))

  x <- array(integer(), c(0L, 0L, 1L))
  expect_identical(rray_remove_axes(x, 3L), array(integer(), c(0L, 0L)))

  x <- array(integer(), c(0L, 0L))
  expect_identical(rray_remove_axes(x, integer()), x)
})

test_that("surviving axes keep their names", {
  x <- array(
    1:6,
    c(1L, 2L, 1L, 3L),
    dimnames = list("drop1", c("a", "b"), "drop3", c("x", "y", "z"))
  )
  expected <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("x", "y", "z"))
  )

  expect_identical(rray_remove_axes(x, c(1L, 3L)), expected)
})

test_that("surviving unnamed axes remain unnamed", {
  x <- array(
    1:6,
    c(1L, 2L, 1L, 3L),
    dimnames = list("drop1", c("a", "b"), "drop3", NULL)
  )
  expected <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))

  expect_identical(rray_remove_axes(x, c(1L, 3L)), expected)
})

test_that("removed axes drop their names", {
  x <- array(1:5, c(1L, 5L), dimnames = list("drop", letters[1:5]))
  expected <- array(1:5, 5L, dimnames = list(letters[1:5]))
  expect_identical(rray_remove_axes(x, 1L), expected)

  x <- array(1L, c(1L, 1L), dimnames = list("drop1", "drop2"))
  expect_identical(
    rray_remove_axes(x, 1L),
    array(1L, 1L, dimnames = list("drop2"))
  )

  x <- array(1:2, c(1L, 2L), dimnames = list("drop", NULL))
  expect_identical(rray_remove_axes(x, 1L), array(1:2, 2L))
})

test_that("does not modify the input", {
  x <- array(1L, c(1L, 1L), dimnames = list("a", "b"))
  expected <- x
  rray_remove_axes(x, 2L)

  expect_identical(x, expected)
})

test_that("axes are coerced to integer", {
  x <- array(1:3, c(3L, 1L))
  expect_identical(rray_remove_axes(x, 2), array(1:3, 3L))
})

test_that("errors when a selected axis does not have dimension 1", {
  expect_snapshot(error = TRUE, {
    rray_remove_axes(array(1:2, c(2L, 1L)), 1L)
    rray_remove_axes(array(integer(), c(0L, 1L)), 1L)
  })
})

test_that("errors when every axis is removed", {
  expect_snapshot(error = TRUE, {
    rray_remove_axes(array(1L, c(1L, 1L, 1L)), c(1L, 2L, 3L))
    rray_remove_axes(array(1L, 1L), 1L)
  })
})

test_that("errors on invalid axes", {
  x <- array(1L, c(1L, 1L))

  expect_snapshot(error = TRUE, {
    rray_remove_axes(x, 3L)
    rray_remove_axes(x, 0L)
    rray_remove_axes(x, NA_integer_)
    rray_remove_axes(x, c(1L, 1L))
    rray_remove_axes(x, c(2L, 1L))
    rray_remove_axes(x, structure(1L, foo = "bar"))
    rray_remove_axes(x, 1.5)
    rray_remove_axes(x, "x")
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_remove_axes(NULL, 1L)
    rray_remove_axes(mean, 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_remove_axes(x, 1L), error = TRUE)
})
