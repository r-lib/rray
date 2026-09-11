test_that("transposes a matrix", {
  x <- array(1:6, c(2L, 3L))
  expected <- array(c(1L, 3L, 5L, 2L, 4L, 6L), c(3L, 2L))

  expect_identical(rray_permute_axes(x, c(2L, 1L)), expected)
})

test_that("axis `i` of the result comes from axis `axes[i]` of `x`", {
  x <- array(1:24, c(4L, 3L, 2L))
  out <- rray_permute_axes(x, c(2L, 3L, 1L))

  expect_identical(rray_dimensions(out), c(3L, 2L, 4L))
  expect_identical(out[2L, 1L, 3L], x[3L, 2L, 1L])
})

test_that("an identity permutation returns the input", {
  x <- array(1:24, c(4L, 3L, 2L))
  expect_identical(rray_permute_axes(x, c(1L, 2L, 3L)), x)
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
    expect_identical(rray_permute_axes(x, permutation), aperm(x, permutation))
  }
})

test_that("matches `aperm()` for a 4D array", {
  x <- array(seq_len(7L * 5L * 3L * 2L), c(7L, 5L, 3L, 2L))

  expect_identical(
    rray_permute_axes(x, c(4L, 3L, 2L, 1L)),
    aperm(x, c(4L, 3L, 2L, 1L))
  )
  expect_identical(
    rray_permute_axes(x, c(3L, 1L, 4L, 2L)),
    aperm(x, c(3L, 1L, 4L, 2L))
  )
})

test_that("names travel with their axis", {
  x <- array(
    1:6,
    c(1L, 2L, 3L),
    dimnames = list("a", c("b", "c"), c("d", "e", "f"))
  )
  out <- rray_permute_axes(x, c(3L, 1L, 2L))

  expect_identical(rray_names(out), list(c("d", "e", "f"), "a", c("b", "c")))
})

test_that("unnamed axes remain unnamed", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  out <- rray_permute_axes(x, c(2L, 1L))

  expect_identical(rray_names(out), list(NULL, c("a", "b")))
})

test_that("outer names of `dimnames()` are dropped", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(rows = c("a", "b"), cols = c("x", "y", "z"))
  )

  expect_named(dimnames(rray_permute_axes(x, c(2L, 1L))), NULL)
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
    expect_identical(rray_permute_axes(x, c(2L, 1L)), aperm(x))
  }
})

test_that("works with zero-size arrays", {
  x <- array(integer(), c(0L, 3L))
  expect_identical(
    rray_permute_axes(x, c(2L, 1L)),
    array(integer(), c(3L, 0L))
  )

  x <- array(integer(), c(0L, 0L, 2L))
  expect_identical(
    rray_permute_axes(x, c(3L, 2L, 1L)),
    array(integer(), c(2L, 0L, 0L))
  )
})

test_that("1D arrays come back unchanged", {
  expect_identical(rray_permute_axes(array(1:3, 3L), 1L), array(1:3, 3L))

  expected <- array(1:2, 2L, dimnames = list(c("a", "b")))
  expect_identical(rray_permute_axes(c(a = 1L, b = 2L), 1L), expected)
})

test_that("does not modify the input", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))
  expected <- x
  rray_permute_axes(x, c(2L, 1L))

  expect_identical(x, expected)
})

test_that("errors on invalid `axes`", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_permute_axes(x, NULL)
    rray_permute_axes(x, 1L)
    rray_permute_axes(x, c(1L, 2L, 3L))
    rray_permute_axes(x, c(1L, 1L))
    rray_permute_axes(x, c(1L, 3L))
    rray_permute_axes(x, c(1L, 0L))
    rray_permute_axes(x, c(1L, NA_integer_))
    rray_permute_axes(x, structure(c(1L, 2L), foo = "bar"))
    rray_permute_axes(x, c(1.5, 2))
    rray_permute_axes(x, "x")
  })
})

test_that("errors on non-array input", {
  expect_snapshot(error = TRUE, {
    rray_permute_axes(NULL, 1L)
    rray_permute_axes(mean, 1L)
  })
})

test_that("errors on classed input", {
  x <- structure(array(1L), class = "foo")
  expect_snapshot(rray_permute_axes(x, 1L), error = TRUE)
})
