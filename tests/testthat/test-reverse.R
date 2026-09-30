test_that("reverses along every axis", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_reverse(x, axes = 1),
    rray_slice_axis(x, 2:1, axis = 1)
  )
  expect_identical(
    rray_reverse(x, axes = 2),
    rray_slice_axis(x, 3:1, axis = 2)
  )
  expect_identical(
    rray_reverse(x, axes = 3),
    rray_slice_axis(x, 4:1, axis = 3)
  )
})

test_that("reverses several axes at once", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_reverse(x, axes = c(1, 3)),
    rray_slice(x, 2:1, TRUE, 4:1)
  )
  expect_identical(
    rray_reverse(x, axes = c(1, 2, 3)),
    rray_slice(x, 2:1, 3:1, 4:1)
  )
})

test_that("reversing several axes is reversing them one at a time", {
  x <- array(1:60, c(3, 4, 5))

  expect_identical(
    rray_reverse(x, axes = c(1, 3)),
    rray_reverse(rray_reverse(x, axes = 1), axes = 3)
  )
})

test_that("reversing twice gives back `x`", {
  x <- array(1:60, c(3, 4, 5))
  dimnames(x) <- list(letters[1:3], letters[4:7], letters[8:12])

  for (axis in 1:3) {
    out <- rray_reverse(x, axes = axis)
    expect_identical(rray_reverse(out, axes = axis), x)
  }
})

test_that("works with every type", {
  inputs <- list(
    c(TRUE, NA, FALSE),
    c(1L, NA, 3L),
    c(1.5, NA, 3.5),
    c(1 + 1i, NA, 3 + 3i),
    as.raw(1:3),
    c("a", NA, "c"),
    list(1, "a", NULL)
  )

  for (input in inputs) {
    x <- array(input, c(3, 2))
    expect_identical(rray_reverse(x, axes = 1), rray_slice(x, 3:1, TRUE))
    expect_identical(rray_reverse(x, axes = 2), rray_slice(x, TRUE, 2:1))
  }
})

test_that("works with bare vectors", {
  expect_identical(rray_reverse(1:5, axes = 1), array(5:1, 5))

  expect_identical(
    rray_reverse(c(a = 1L, b = 2L, c = 3L), axes = 1),
    array(3:1, 3, dimnames = list(c("c", "b", "a")))
  )
})

test_that("names reverse on reversed axes and are untouched elsewhere", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

  expect_identical(
    dimnames(rray_reverse(x, axes = 1)),
    list(c("r2", "r1"), c("a", "b", "c"))
  )
  expect_identical(
    dimnames(rray_reverse(x, axes = 2)),
    list(c("r1", "r2"), c("c", "b", "a"))
  )
  expect_identical(
    dimnames(rray_reverse(x, axes = c(1, 2))),
    list(c("r2", "r1"), c("c", "b", "a"))
  )
})

test_that("partial and missing dimension names are handled", {
  x <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))

  expect_identical(
    dimnames(rray_reverse(x, axes = 1)),
    list(NULL, c("a", "b", "c"))
  )
  expect_identical(
    dimnames(rray_reverse(x, axes = 2)),
    list(NULL, c("c", "b", "a"))
  )

  expect_null(dimnames(rray_reverse(matrix(1:6, nrow = 2), axes = 1)))
})

test_that("returns `x` when nothing moves", {
  x <- matrix(1:3, nrow = 1, dimnames = list("r1", c("a", "b", "c")))

  expect_identical(rray_reverse(x, axes = 1), x)
  expect_identical(rray_reverse(x, axes = integer()), x)
})

test_that("works with a zero dimension", {
  x <- array(integer(), c(2, 0))
  expect_identical(rray_reverse(x, axes = 2), x)
  expect_identical(rray_reverse(x, axes = c(1, 2)), x)

  x <- array(integer(), 0)
  expect_identical(rray_reverse(x, axes = 1), x)
})

test_that("names still reverse with a zero dimension on another axis", {
  x <- array(integer(), c(3, 0), dimnames = list(c("a", "b", "c"), NULL))

  expect_identical(
    rray_reverse(x, axes = 1),
    array(integer(), c(3, 0), dimnames = list(c("c", "b", "a"), NULL))
  )
})

test_that("`axes` can be integerish doubles or logicals", {
  x <- array(1:24, c(2, 3, 4))

  expect_identical(
    rray_reverse(x, axes = c(2, 3)),
    rray_reverse(x, axes = c(2L, 3L))
  )
  expect_identical(
    rray_reverse(x, axes = TRUE),
    rray_reverse(x, axes = 1L)
  )
})

test_that("`axes` is validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_reverse(x, axes = c(1, 1)), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = c(2, 1)), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = 3), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = 0), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = NA), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = 1.5), error = TRUE)
  expect_snapshot(rray_reverse(x, axes = "a"), error = TRUE)
})

test_that("errors on invalid input", {
  expect_snapshot(rray_reverse(NULL, axes = 1), error = TRUE)

  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_reverse(x, axes = 1), error = TRUE)
})
