test_that("moves a single axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_move_axes(x, from = 1L, to = 3L),
    rray_permute_axes(x, c(2L, 3L, 1L))
  )
  expect_identical(
    rray_move_axes(x, from = 3L, to = 1L),
    rray_permute_axes(x, c(3L, 1L, 2L))
  )
})

test_that("`from[[i]]` ends up at position `to[[i]]`", {
  x <- array(seq_len(7L * 5L * 3L * 2L), c(7L, 5L, 3L, 2L))
  out <- rray_move_axes(x, from = c(1L, 2L), to = c(4L, 3L))

  expect_identical(rray_dimensions(out), c(3L, 2L, 5L, 7L))
  expect_identical(out, rray_permute_axes(x, c(3L, 4L, 2L, 1L)))
})

test_that("axes that don't move keep their relative order", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_dimensions(rray_move_axes(x, from = 2L, to = 1L)),
    c(3L, 2L, 4L)
  )
  expect_identical(
    rray_dimensions(rray_move_axes(x, from = c(1L, 3L), to = c(2L, 1L))),
    c(4L, 2L, 3L)
  )
})

test_that("an identity move returns the input", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_move_axes(x, from = 2L, to = 2L), x)
  expect_identical(
    rray_move_axes(x, from = c(1L, 2L, 3L), to = c(1L, 2L, 3L)),
    x
  )
})

test_that("moving no axes returns the input", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_identical(rray_move_axes(x, from = integer(), to = integer()), x)
})

test_that("names travel with their axis", {
  x <- array(
    1:6,
    c(1L, 2L, 3L),
    dimnames = list("a", c("b", "c"), c("d", "e", "f"))
  )
  out <- rray_move_axes(x, from = 3L, to = 1L)

  expect_identical(rray_names(out), list(c("d", "e", "f"), "a", c("b", "c")))
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 3L, 2L))

  expect_identical(
    rray_move_axes(x, from = 1L, to = 3L),
    array(integer(), c(3L, 2L, 0L))
  )
})

test_that("1D arrays come back unchanged", {
  expect_identical(
    rray_move_axes(array(1:3, 3L), from = 1L, to = 1L),
    array(1:3, 3L)
  )

  expected <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_identical(
    rray_move_axes(c(a = 1L, b = 2L), from = 1L, to = 1L),
    expected
  )
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
      rray_move_axes(x, from = 1L, to = 2L),
      rray_permute_axes(x, c(2L, 1L))
    )
  }
})

test_that("`from` and `to` must be named", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, 1L, 3L)
    rray_move_axes(x, 1L, to = 3L)
  })
})

test_that("errors on invalid `from`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, from = c(1L, 1L), to = c(1L, 2L))
    rray_move_axes(x, from = 4L, to = 1L)
    rray_move_axes(x, from = 0L, to = 1L)
    rray_move_axes(x, from = NA_integer_, to = 1L)
    rray_move_axes(x, from = structure(1L, foo = "bar"), to = 1L)
    rray_move_axes(x, from = 1.5, to = 1L)
    rray_move_axes(x, from = "x", to = 1L)
  })
})

test_that("errors on invalid `to`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, from = c(1L, 2L), to = c(3L, 3L))
    rray_move_axes(x, from = 1L, to = 4L)
    rray_move_axes(x, from = 1L, to = 0L)
    rray_move_axes(x, from = 1L, to = NA_integer_)
    rray_move_axes(x, from = 1L, to = structure(1L, foo = "bar"))
    rray_move_axes(x, from = 1L, to = 1.5)
    rray_move_axes(x, from = 1L, to = "x")
  })
})

test_that("`from` and `to` must be the same length", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_move_axes(x, from = c(1L, 2L), to = 1L)
    rray_move_axes(x, from = 1L, to = c(1L, 2L))
    rray_move_axes(x, from = 1L, to = integer())
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_move_axes(NULL, from = 1L, to = 1L)
    rray_move_axes(mean, from = 1L, to = 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_move_axes(x, from = 1L, to = 1L), error = TRUE)
})
