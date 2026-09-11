test_that("transposes a matrix", {
  x <- array(1:6, c(2L, 3L))
  expected <- array(c(1L, 3L, 5L, 2L, 4L, 6L), c(3L, 2L))

  expect_identical(rray_transpose(x), expected)
})

test_that("`permutation = NULL` reverses the axes", {
  x <- array(1:24, c(4L, 3L, 2L))

  expect_identical(rray_transpose(x), rray_transpose(x, c(3L, 2L, 1L)))
  expect_identical(rray_dimensions(rray_transpose(x)), c(2L, 3L, 4L))
})

test_that("axis `i` of the result comes from axis `permutation[i]` of `x`", {
  x <- array(1:24, c(4L, 3L, 2L))
  out <- rray_transpose(x, c(2L, 3L, 1L))

  expect_identical(rray_dimensions(out), c(3L, 2L, 4L))
  expect_identical(out[2L, 1L, 3L], x[3L, 2L, 1L])
})

test_that("an identity `permutation` returns the input", {
  x <- array(1:24, c(4L, 3L, 2L))
  expect_identical(rray_transpose(x, c(1L, 2L, 3L)), x)
})

test_that("matches `aperm()` for every permutation of a 3D array", {
  x <- array(1:24, c(4L, 3L, 2L))

  permutations <- list(
    c(1L, 2L, 3L),
    c(1L, 3L, 2L),
    c(2L, 1L, 3L),
    c(2L, 3L, 1L),
    c(3L, 1L, 2L),
    c(3L, 2L, 1L)
  )

  for (permutation in permutations) {
    expect_identical(rray_transpose(x, permutation), aperm(x, permutation))
  }
})

test_that("matches `aperm()` for a 4D array", {
  x <- array(seq_len(7L * 5L * 3L * 2L), c(7L, 5L, 3L, 2L))

  expect_identical(rray_transpose(x), aperm(x))
  expect_identical(
    rray_transpose(x, c(3L, 1L, 4L, 2L)),
    aperm(x, c(3L, 1L, 4L, 2L))
  )
})

test_that("names travel with their axis", {
  x <- array(
    1:6,
    c(1L, 2L, 3L),
    dimnames = list("a", c("b", "c"), c("d", "e", "f"))
  )
  out <- rray_transpose(x, c(3L, 1L, 2L))

  expect_identical(rray_names(out), list(c("d", "e", "f"), "a", c("b", "c")))
})

test_that("unnamed axes remain unnamed", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  expect_identical(rray_names(rray_transpose(x)), list(NULL, c("a", "b")))
})

test_that("outer names of `dimnames()` are dropped", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(rows = c("a", "b"), cols = c("x", "y", "z"))
  )

  expect_named(dimnames(rray_transpose(x)), NULL)
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
    expect_identical(rray_transpose(x), aperm(x))
  }
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 3L))
  expect_identical(rray_transpose(x), array(integer(), c(3L, 0L)))

  x <- array(integer(), c(0L, 0L, 2L))
  expect_identical(rray_transpose(x), array(integer(), c(2L, 0L, 0L)))
})

test_that("1D arrays come back unchanged", {
  expect_identical(rray_transpose(array(1:3, 3L)), array(1:3, 3L))

  expected <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_identical(rray_transpose(c(a = 1L, b = 2L)), expected)
})

test_that("`permutation` is coerced to integer", {
  x <- array(1:6, c(2L, 3L))
  expect_identical(rray_transpose(x, c(2, 1)), rray_transpose(x, c(2L, 1L)))
})

test_that("does not modify the input", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  expected <- x
  rray_transpose(x)

  expect_identical(x, expected)
})

test_that("errors on invalid `permutation`", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_transpose(x, 1L)
    rray_transpose(x, c(1L, 2L, 3L))
    rray_transpose(x, c(1L, 1L))
    rray_transpose(x, c(1L, 3L))
    rray_transpose(x, c(1L, 0L))
    rray_transpose(x, c(1L, NA_integer_))
    rray_transpose(x, structure(c(1L, 2L), foo = "bar"))
    rray_transpose(x, c(1.5, 2))
    rray_transpose(x, "x")
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_transpose(NULL)
    rray_transpose(mean)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_transpose(x), error = TRUE)
})
