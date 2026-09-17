test_that("moves a single axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_move_axes(x, 1L, 3L),
    rray_permute_axes(x, c(2L, 3L, 1L))
  )
  expect_identical(
    rray_move_axes(x, 3L, 1L),
    rray_permute_axes(x, c(3L, 1L, 2L))
  )
})

test_that("`from[[i]]` ends up at position `to[[i]]`", {
  x <- array(seq_len(7L * 5L * 3L * 2L), c(7L, 5L, 3L, 2L))
  out <- rray_move_axes(x, c(1L, 2L), c(4L, 3L))

  expect_identical(rray_dimensions(out), c(3L, 2L, 5L, 7L))
  expect_identical(out, rray_permute_axes(x, c(3L, 4L, 2L, 1L)))
})

test_that("axes that don't move keep their relative order", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_dimensions(rray_move_axes(x, 2L, 1L)), c(3L, 2L, 4L))
  expect_identical(
    rray_dimensions(rray_move_axes(x, c(1L, 3L), c(2L, 1L))),
    c(4L, 2L, 3L)
  )
})

test_that("an identity move returns the input", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_move_axes(x, 2L, 2L), x)
  expect_identical(rray_move_axes(x, c(1L, 2L, 3L), c(1L, 2L, 3L)), x)
})

test_that("moving no axes returns the input", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_identical(rray_move_axes(x, integer(), integer()), x)
})

test_that("names travel with their axis", {
  x <- array(
    1:6,
    c(1L, 2L, 3L),
    dimnames = list("a", c("b", "c"), c("d", "e", "f"))
  )
  out <- rray_move_axes(x, 3L, 1L)

  expect_identical(rray_names(out), list(c("d", "e", "f"), "a", c("b", "c")))
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 3L, 2L))

  expect_identical(
    rray_move_axes(x, 1L, 3L),
    array(integer(), c(3L, 2L, 0L))
  )
})

test_that("1D arrays come back unchanged", {
  expect_identical(rray_move_axes(array(1:3, 3L), 1L, 1L), array(1:3, 3L))

  expected <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_identical(rray_move_axes(c(a = 1L, b = 2L), 1L, 1L), expected)
})

test_that("works with every native type", {
  xs <- list(
    array(c(TRUE, FALSE), c(1L, 2L)),
    array(1:2, c(1L, 2L)),
    array(c(1, 2), c(1L, 2L)),
    array(c(1i, 2i), c(1L, 2L)),
    array(c("x", "y"), c(1L, 2L)),
    array(as.raw(1:2), c(1L, 2L)),
    array(list("x", "y"), c(1L, 2L))
  )

  for (x in xs) {
    expect_identical(
      rray_move_axes(x, 1L, 2L),
      rray_permute_axes(x, c(2L, 1L))
    )
  }
})

test_that("errors on invalid `from`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, c(1L, 1L), c(1L, 2L))
    rray_move_axes(x, 4L, 1L)
    rray_move_axes(x, 0L, 1L)
    rray_move_axes(x, NA_integer_, 1L)
    rray_move_axes(x, structure(1L, foo = "bar"), 1L)
    rray_move_axes(x, 1.5, 1L)
    rray_move_axes(x, "x", 1L)
  })
})

test_that("errors on invalid `to`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, c(1L, 2L), c(3L, 3L))
    rray_move_axes(x, 1L, 4L)
    rray_move_axes(x, 1L, 0L)
    rray_move_axes(x, 1L, NA_integer_)
    rray_move_axes(x, 1L, structure(1L, foo = "bar"))
    rray_move_axes(x, 1L, 1.5)
    rray_move_axes(x, 1L, "x")
  })
})

test_that("`from` and `to` must be the same length", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, c(1L, 2L), 1L)
    rray_move_axes(x, 1L, c(1L, 2L))
    rray_move_axes(x, 1L, integer())
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_move_axes(NULL, 1L, 1L)
    rray_move_axes(mean, 1L, 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_move_axes(x, 1L, 1L), error = TRUE)
})
