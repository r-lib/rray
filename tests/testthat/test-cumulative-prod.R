test_that("can accumulate along axis 1", {
  x <- array(c(2, 3, -1, 4, 0, 5), c(3L, 2L))

  expect_identical(
    rray_cumulative_prod(x, 1L),
    array(c(2, 6, -6, 4, 0, 0), c(3L, 2L))
  )
})

test_that("can accumulate along axis 2", {
  x <- array(c(2, 4, 3, 0, -1, 5), c(2L, 3L))

  expect_identical(
    rray_cumulative_prod(x, 2L),
    array(c(2, 4, 6, 0, -6, 0), c(2L, 3L))
  )
})

test_that("can accumulate along every axis of a 3D array", {
  x <- array(rep_len(c(2L, -1L, 3L, 1L, -2L), 24L), c(2L, 3L, 4L))
  expected <- array(as.double(x), dim(x))

  for (axis in 1:3) {
    expect_identical(
      rray_cumulative_prod(x, axis),
      cumulative_reference(expected, axis, `*`)
    )
  }
})

test_that("each line starts fresh", {
  x <- array(c(0, 2, 3, 4), c(2L, 2L))
  expect_identical(
    rray_cumulative_prod(x, 1L),
    array(c(0, 0, 3, 12), c(2L, 2L))
  )
  expect_identical(
    rray_cumulative_prod(x, 2L),
    array(c(0, 2, 0, 8), c(2L, 2L))
  )
})

test_that("logicals and integers become doubles", {
  expect_identical(
    rray_cumulative_prod(c(TRUE, TRUE, FALSE), 1L),
    array(c(1, 1, 0), 3L)
  )
  expect_identical(
    rray_cumulative_prod(c(2L, -3L, 4L), 1L),
    array(c(2, -6, -24), 3L)
  )
  expect_identical(
    rray_cumulative_prod(c(2, 0.5), 1L),
    array(c(2, 1), 2L)
  )
  expect_identical(
    rray_cumulative_prod(c(1i, 1i), 1L),
    array(c(1i, -1 + 0i), 2L)
  )
})

test_that("integers do not overflow", {
  x <- c(.Machine$integer.max, .Machine$integer.max)
  expect_identical(
    rray_cumulative_prod(x, 1L),
    array(c(.Machine$integer.max, .Machine$integer.max^2), 2L)
  )
})

test_that("matches base R for simple lines", {
  lines <- list(
    c(TRUE, TRUE, FALSE, TRUE),
    c(2L, -1L, 3L, 0L, 5L),
    c(0.5, -4, 2, -0.25),
    c(1 + 1i, 2 - 1i, 0 + 1i, -3 + 0i)
  )

  for (x in lines) {
    expect_identical(as.vector(rray_cumulative_prod(x, 1L)), cumprod(x))
  }
})

test_that("products can overflow to infinity and underflow to zero", {
  expect_identical(
    as.vector(rray_cumulative_prod(c(1e200, -1e200, 2), 1L)),
    c(1e200, -Inf, -Inf)
  )
  expect_identical(
    as.vector(rray_cumulative_prod(c(1e-200, 1e-200, 2), 1L)),
    c(1e-200, 0, 0)
  )
})

test_that("zero times infinity is `NaN`", {
  expect_identical(
    as.vector(rray_cumulative_prod(c(0, Inf, 1), 1L)),
    c(0, NaN, NaN)
  )
  expect_identical(
    as.vector(rray_cumulative_prod(c(Inf, 0, 1), 1L)),
    c(Inf, NaN, NaN)
  )
})

test_that("zero keeps its sign", {
  out <- as.vector(rray_cumulative_prod(c(-1, 0, 2, -3), 1L))
  expect_identical(1 / out, c(-1, -Inf, -Inf, Inf))
})

test_that("missing values persist through the rest of a line", {
  expect_identical(
    as.vector(rray_cumulative_prod(c(TRUE, NA, FALSE), 1L)),
    c(1, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_prod(c(2L, NA, 0L), 1L)),
    c(2, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_prod(c(2, NA, 0), 1L)),
    c(2, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_prod(c(2, NaN, 0), 1L)),
    c(2, NaN, NaN)
  )
  expect_identical(
    is.na(as.vector(rray_cumulative_prod(c(1i, NA, 0i), 1L))),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("`NA` and `NaN` together are missing in either order", {
  expect_identical(
    is.na(as.vector(rray_cumulative_prod(c(2, NA, NaN, 3), 1L))),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    is.na(as.vector(rray_cumulative_prod(c(2, NaN, NA, 3), 1L))),
    c(FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("last position matches the reducer", {
  x <- array(c(2, -1, 0.5, 4, -3, 0.25), c(2L, 3L))
  expect_identical(
    rray_cumulative_prod(x, 2L)[, 3L, drop = FALSE],
    rray_prod(x, 2L)
  )

  x <- array(c(1 + 1i, 2i, -1 + 0i, 3 - 1i), c(2L, 2L))
  expect_identical(
    rray_cumulative_prod(x, 1L)[2L, , drop = FALSE],
    rray_prod(x, 1L)
  )
})

test_that("an axis of dimension 1 still casts to double", {
  x <- array(c(TRUE, FALSE, NA), c(1L, 3L))
  expect_identical(rray_cumulative_prod(x, 1L), array(c(1, 0, NA), c(1L, 3L)))

  x <- array(c(-2L, 3L, NA), c(1L, 3L))
  expect_identical(rray_cumulative_prod(x, 1L), array(c(-2, 3, NA), c(1L, 3L)))

  x <- array(c(-Inf, 2, NA, NaN), c(1L, 4L))
  expect_identical(rray_cumulative_prod(x, 1L), x)
})

test_that("zero-size arrays keep their shape", {
  for (x in list(logical(), integer(), double())) {
    x <- array(x, c(2L, 0L, 3L))

    for (axis in 1:3) {
      expect_identical(
        rray_cumulative_prod(x, axis),
        array(double(), c(2L, 0L, 3L))
      )
    }
  }

  x <- array(complex(), c(2L, 0L, 3L))
  expect_identical(rray_cumulative_prod(x, 2L), x)
})

test_that("names are kept on every axis", {
  x <- array(
    c(1, 2, 3, 4),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("c", "d"))
  )
  expect_identical(dimnames(rray_cumulative_prod(x, 1L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_prod(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L), dimnames = list(NULL, c("c", "d")))
  expect_identical(dimnames(rray_cumulative_prod(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L))
  expect_null(dimnames(rray_cumulative_prod(x, 1L)))
})

test_that("titles are kept", {
  x <- array(
    1L,
    c(2L, 2L),
    dimnames = list(Row = c("a", "b"), Column = NULL)
  )
  expect_identical(
    rray_titles(rray_cumulative_prod(x, 1L)),
    c("Row", "Column")
  )
})

test_that("vectors become 1D arrays", {
  expect_identical(
    rray_cumulative_prod(c(a = 1, b = 3, c = 2), 1L),
    array(c(1, 3, 6), 3L, dimnames = list(c("a", "b", "c")))
  )
  expect_identical(
    rray_cumulative_prod(c(3L, 1L, 2L), 1L),
    array(c(3, 3, 6), 3L)
  )
})

test_that("`axis` can be a whole double", {
  x <- array(c(1, 2), c(2L, 2L))
  expect_identical(rray_cumulative_prod(x, 2), rray_cumulative_prod(x, 2L))
})

test_that("`axis` is validated", {
  x <- array(1, c(2L, 2L))

  expect_snapshot(rray_cumulative_prod(x, integer()), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, 1:2), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, 0L), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, -1L), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, 1.5), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, 3L), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, structure(1L, foo = 1)), error = TRUE)
  expect_snapshot(rray_cumulative_prod(x, "a"), error = TRUE)
})

test_that("errors on unsupported input types", {
  expect_snapshot(rray_cumulative_prod("a", 1L), error = TRUE)
  expect_snapshot(rray_cumulative_prod(as.raw(1L), 1L), error = TRUE)
  expect_snapshot(rray_cumulative_prod(list(1), 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_cumulative_prod(x, 1L), error = TRUE)
})
