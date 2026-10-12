test_that("can accumulate along axis 1", {
  x <- array(c(2L, 3L, 1L, 6L, 4L, 5L), c(3L, 2L))

  expect_identical(
    rray_cumulative_max(x, 1L),
    array(c(2L, 3L, 3L, 6L, 6L, 6L), c(3L, 2L))
  )
  expect_identical(
    rray_cumulative_min(x, 1L),
    array(c(2L, 2L, 1L, 6L, 4L, 4L), c(3L, 2L))
  )
})

test_that("can accumulate along axis 2", {
  x <- array(c(2L, 6L, 3L, 4L, 1L, 5L), c(2L, 3L))

  expect_identical(
    rray_cumulative_max(x, 2L),
    array(c(2L, 6L, 3L, 6L, 3L, 6L), c(2L, 3L))
  )
  expect_identical(
    rray_cumulative_min(x, 2L),
    array(c(2L, 6L, 2L, 4L, 1L, 4L), c(2L, 3L))
  )
})

test_that("can accumulate along every axis of a 3D array", {
  x <- array((1:24 * 5L) %% 24L, c(2L, 3L, 4L))

  for (axis in 1:3) {
    expect_identical(
      rray_cumulative_max(x, axis),
      cumulative_reference(x, axis, max)
    )
    expect_identical(
      rray_cumulative_min(x, axis),
      cumulative_reference(x, axis, min)
    )
  }
})

test_that("each line starts fresh", {
  x <- array(c(5, 1, 2, 3), c(2L, 2L))
  expect_identical(
    rray_cumulative_max(x, 1L),
    array(c(5, 5, 2, 3), c(2L, 2L))
  )
  expect_identical(
    rray_cumulative_min(x, 1L),
    array(c(5, 1, 2, 2), c(2L, 2L))
  )
})

test_that("output type is the input type", {
  expect_identical(
    rray_cumulative_max(c(FALSE, TRUE), 1L),
    array(c(FALSE, TRUE), 2L)
  )
  expect_identical(
    rray_cumulative_min(c(TRUE, FALSE), 1L),
    array(c(TRUE, FALSE), 2L)
  )
  expect_identical(rray_cumulative_max(c(1L, 2L), 1L), array(c(1L, 2L), 2L))
  expect_identical(rray_cumulative_min(c(2L, 1L), 1L), array(c(2L, 1L), 2L))
  expect_identical(rray_cumulative_max(c(1, 2), 1L), array(c(1, 2), 2L))
  expect_identical(rray_cumulative_min(c(2, 1), 1L), array(c(2, 1), 2L))
})

test_that("matches base R for rising and falling lines", {
  lines <- list(
    c(1L, 3L, 2L, 5L, 4L),
    c(5L, 3L, 4L, 1L, 2L),
    c(1.5, -2, Inf, 0, -Inf),
    c(-Inf, 2, 2, -1, 3.5)
  )

  for (x in lines) {
    expect_identical(as.vector(rray_cumulative_max(x, 1L)), cummax(x))
    expect_identical(as.vector(rray_cumulative_min(x, 1L)), cummin(x))
  }
})

test_that("matches repeated `pmax()` and `pmin()` across special values", {
  cases <- list(
    list(values = c(NA, FALSE, TRUE), cast = as.logical),
    list(values = c(NA, -1L, 0L, 1L), cast = as.integer),
    list(values = c(NA, NaN, -Inf, -1, -0, 0, 1, Inf), cast = as.double)
  )

  pmax_na <- \(x, y) na_wins(pmax(x, y), x, y)
  pmin_na <- \(x, y) na_wins(pmin(x, y), x, y)

  for (case in cases) {
    lines <- expand.grid(rep(list(case$values), 3L))

    for (i in seq_len(nrow(lines))) {
      x <- unlist(lines[i, ], use.names = FALSE)

      expect_identical(
        as.vector(rray_cumulative_max(x, 1L)),
        case$cast(Reduce(pmax_na, x, accumulate = TRUE))
      )
      expect_identical(
        as.vector(rray_cumulative_min(x, 1L)),
        case$cast(Reduce(pmin_na, x, accumulate = TRUE))
      )
    }
  }
})

test_that("missing values persist through the rest of a line", {
  expect_identical(
    as.vector(rray_cumulative_max(c(1L, NA, 2L), 1L)),
    c(1L, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_min(c(TRUE, NA, FALSE), 1L)),
    c(TRUE, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_max(c(1, NaN, 2, NA, 3), 1L)),
    c(1, NaN, NaN, NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_min(c(1, NA, 0, NaN, -1), 1L)),
    c(1, NA, NA, NA, NA)
  )
})

test_that("a later `TRUE` or `FALSE` does not settle an earlier `NA`", {
  expect_identical(
    as.vector(rray_cumulative_max(c(NA, TRUE), 1L)),
    c(NA, NA)
  )
  expect_identical(
    as.vector(rray_cumulative_min(c(NA, FALSE), 1L)),
    c(NA, NA)
  )
})

test_that("last position matches the reducer", {
  x <- array(c(3, -1, Inf, 2, 0, -Inf), c(2L, 3L))

  expect_identical(
    rray_cumulative_max(x, 2L)[, 3L, drop = FALSE],
    rray_max(x, axes = 2L)
  )
  expect_identical(
    rray_cumulative_min(x, 2L)[, 3L, drop = FALSE],
    rray_min(x, 2L)
  )
})

test_that("an axis of dimension 1 returns `x`", {
  x <- array(c(TRUE, FALSE, NA), c(1L, 3L))
  expect_identical(rray_cumulative_max(x, 1L), x)
  expect_identical(rray_cumulative_min(x, 1L), x)

  x <- array(c(-.Machine$integer.max, .Machine$integer.max, NA), c(1L, 3L))
  expect_identical(rray_cumulative_max(x, 1L), x)
  expect_identical(rray_cumulative_min(x, 1L), x)

  x <- array(c(-Inf, Inf, NA, NaN), c(1L, 4L))
  expect_identical(rray_cumulative_max(x, 1L), x)
  expect_identical(rray_cumulative_min(x, 1L), x)
})

test_that("zero-size arrays keep their shape", {
  for (x in list(logical(), integer(), double())) {
    x <- array(x, c(2L, 0L, 3L))

    for (axis in 1:3) {
      expect_identical(rray_cumulative_max(x, axis), x)
      expect_identical(rray_cumulative_min(x, axis), x)
    }
  }
})

test_that("names are kept on every axis", {
  x <- array(
    c(1L, 2L, 3L, 4L),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("c", "d"))
  )
  expect_identical(dimnames(rray_cumulative_max(x, 1L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_min(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L), dimnames = list(NULL, c("c", "d")))
  expect_identical(dimnames(rray_cumulative_max(x, 2L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_min(x, 2L)), dimnames(x))

  x <- array(1L, c(2L, 2L))
  expect_null(dimnames(rray_cumulative_max(x, 1L)))
  expect_null(dimnames(rray_cumulative_min(x, 1L)))
})

test_that("titles are kept", {
  x <- array(
    1L,
    c(2L, 2L),
    dimnames = list(Row = c("a", "b"), Column = NULL)
  )
  expect_identical(rray_titles(rray_cumulative_max(x, 1L)), c("Row", "Column"))
  expect_identical(rray_titles(rray_cumulative_min(x, 2L)), c("Row", "Column"))
})

test_that("vectors become 1D arrays", {
  expect_identical(
    rray_cumulative_max(c(a = 1, b = 3, c = 2), 1L),
    array(c(1, 3, 3), 3L, dimnames = list(c("a", "b", "c")))
  )
  expect_identical(
    rray_cumulative_min(c(3L, 1L, 2L), 1L),
    array(c(3L, 1L, 1L), 3L)
  )
})

test_that("`axis` can be a whole double", {
  x <- array(c(1L, 2L), c(2L, 2L))
  expect_identical(rray_cumulative_max(x, 2), rray_cumulative_max(x, 2L))
  expect_identical(rray_cumulative_min(x, 2), rray_cumulative_min(x, 2L))
})

test_that("`axis` is validated", {
  x <- array(1L, c(2L, 2L))

  expect_snapshot(rray_cumulative_max(x, integer()), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, 1:2), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, NA_integer_), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, 0L), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, -1L), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, 1.5), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, 3L), error = TRUE)
  expect_snapshot(rray_cumulative_max(x, structure(1L, foo = 1)), error = TRUE)
  expect_snapshot(rray_cumulative_min(x, "a"), error = TRUE)
})

test_that("errors on unsupported input types", {
  expect_snapshot(rray_cumulative_max(1i, 1L), error = TRUE)
  expect_snapshot(rray_cumulative_min(1i, 1L), error = TRUE)
  expect_snapshot(rray_cumulative_max("a", 1L), error = TRUE)
  expect_snapshot(rray_cumulative_min(as.raw(1L), 1L), error = TRUE)
  expect_snapshot(rray_cumulative_max(list(1), 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(1L, class = "foo")
  expect_snapshot(rray_cumulative_max(x, 1L), error = TRUE)
  expect_snapshot(rray_cumulative_min(x, 1L), error = TRUE)
})
