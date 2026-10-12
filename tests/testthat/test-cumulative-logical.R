test_that("can accumulate along axis 1", {
  x <- array(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), c(3L, 2L))

  expect_identical(
    rray_cumulative_all(x, axis = 1L),
    array(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), c(3L, 2L))
  )
  expect_identical(
    rray_cumulative_any(x, 1L),
    array(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), c(3L, 2L))
  )
})

test_that("can accumulate along axis 2", {
  x <- array(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), c(2L, 3L))

  expect_identical(
    rray_cumulative_all(x, axis = 2L),
    array(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), c(2L, 3L))
  )
  expect_identical(
    rray_cumulative_any(x, 2L),
    array(c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), c(2L, 3L))
  )
})

test_that("can accumulate along every axis of a 3D array", {
  x <- array(rep_len(c(TRUE, TRUE, NA, TRUE, FALSE), 24L), c(2L, 3L, 4L))

  for (axis in 1:3) {
    expect_identical(
      rray_cumulative_all(x, axis = axis),
      cumulative_reference(x, axis, `&`)
    )
    expect_identical(
      rray_cumulative_any(x, axis),
      cumulative_reference(x, axis, `|`)
    )
  }
})

test_that("each line starts fresh", {
  x <- array(c(FALSE, TRUE, TRUE, TRUE), c(2L, 2L))
  expect_identical(
    rray_cumulative_all(x, axis = 1L),
    array(c(FALSE, FALSE, TRUE, TRUE), c(2L, 2L))
  )

  x <- array(c(TRUE, FALSE, FALSE, FALSE), c(2L, 2L))
  expect_identical(
    rray_cumulative_any(x, 1L),
    array(c(TRUE, TRUE, FALSE, FALSE), c(2L, 2L))
  )
})

test_that("missing values follow `&` and `|`", {
  values <- c(TRUE, FALSE, NA)
  lines <- expand.grid(values, values, values)

  for (i in seq_len(nrow(lines))) {
    x <- unlist(lines[i, ], use.names = FALSE)

    expect_identical(
      as.vector(rray_cumulative_all(x, axis = 1L)),
      Reduce(`&`, x, accumulate = TRUE)
    )
    expect_identical(
      as.vector(rray_cumulative_any(x, 1L)),
      Reduce(`|`, x, accumulate = TRUE)
    )
  }
})

test_that("a later decisive value settles an earlier `NA`", {
  expect_identical(
    as.vector(rray_cumulative_all(c(TRUE, NA, FALSE), axis = 1L)),
    c(TRUE, NA, FALSE)
  )
  expect_identical(
    as.vector(rray_cumulative_all(c(FALSE, NA), axis = 1L)),
    c(FALSE, FALSE)
  )

  expect_identical(
    as.vector(rray_cumulative_any(c(FALSE, NA, TRUE), 1L)),
    c(FALSE, NA, TRUE)
  )
  expect_identical(
    as.vector(rray_cumulative_any(c(TRUE, NA), 1L)),
    c(TRUE, TRUE)
  )
})

test_that("last position matches the reducer", {
  x <- array(c(TRUE, FALSE, NA, TRUE, TRUE, FALSE), c(2L, 3L))

  expect_identical(
    rray_cumulative_all(x, axis = 2L)[, 3L, drop = FALSE],
    rray_all(x, axes = 2L)
  )
  expect_identical(
    rray_cumulative_any(x, 2L)[, 3L, drop = FALSE],
    rray_any(x, axes = 2L)
  )
})

test_that("an axis of dimension 1 returns `x`", {
  x <- array(c(TRUE, FALSE, NA), c(1L, 3L))
  expect_identical(rray_cumulative_all(x, axis = 1L), x)
  expect_identical(rray_cumulative_any(x, 1L), x)
})

test_that("zero-size arrays keep their shape", {
  x <- array(logical(), c(2L, 0L, 3L))

  for (axis in 1:3) {
    expect_identical(rray_cumulative_all(x, axis = axis), x)
    expect_identical(rray_cumulative_any(x, axis), x)
  }
})

test_that("names are kept on every axis", {
  x <- array(
    c(TRUE, FALSE, NA, TRUE),
    c(2L, 2L),
    dimnames = list(c("a", "b"), c("c", "d"))
  )
  expect_identical(dimnames(rray_cumulative_all(x, axis = 1L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_any(x, 2L)), dimnames(x))

  x <- array(TRUE, c(2L, 2L), dimnames = list(NULL, c("c", "d")))
  expect_identical(dimnames(rray_cumulative_all(x, axis = 2L)), dimnames(x))
  expect_identical(dimnames(rray_cumulative_any(x, 2L)), dimnames(x))

  x <- array(TRUE, c(2L, 2L))
  expect_null(dimnames(rray_cumulative_all(x, axis = 1L)))
  expect_null(dimnames(rray_cumulative_any(x, 1L)))
})

test_that("titles are kept", {
  x <- array(
    TRUE,
    c(2L, 2L),
    dimnames = list(Row = c("a", "b"), Column = NULL)
  )
  expect_identical(
    rray_titles(rray_cumulative_all(x, axis = 1L)),
    c("Row", "Column")
  )
  expect_identical(rray_titles(rray_cumulative_any(x, 2L)), c("Row", "Column"))
})

test_that("vectors become 1D arrays", {
  expect_identical(
    rray_cumulative_all(c(a = TRUE, b = NA, c = FALSE), axis = 1L),
    array(c(TRUE, NA, FALSE), 3L, dimnames = list(c("a", "b", "c")))
  )
  expect_identical(
    rray_cumulative_any(c(FALSE, NA, TRUE), 1L),
    array(c(FALSE, NA, TRUE), 3L)
  )
})

test_that("`axis` can be a whole double", {
  x <- array(c(TRUE, FALSE), c(2L, 2L))
  expect_identical(
    rray_cumulative_all(x, axis = 2),
    rray_cumulative_all(x, axis = 2L)
  )
  expect_identical(rray_cumulative_any(x, 2), rray_cumulative_any(x, 2L))
})

test_that("`axis` is validated", {
  x <- array(TRUE, c(2L, 2L))

  expect_snapshot(rray_cumulative_all(x, axis = integer()), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = 1:2), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = NA_integer_), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = 0L), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = -1L), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = 1.5), error = TRUE)
  expect_snapshot(rray_cumulative_all(x, axis = 3L), error = TRUE)
  expect_snapshot(
    rray_cumulative_all(x, axis = structure(1L, foo = 1)),
    error = TRUE
  )
  expect_snapshot(rray_cumulative_any(x, "a"), error = TRUE)
})

test_that("`axis` must be named", {
  expect_snapshot(rray_cumulative_all(c(TRUE, FALSE), 1), error = TRUE)
})

test_that("errors on non-logical input", {
  expect_snapshot(rray_cumulative_all(1:2, axis = 1L), error = TRUE)
  expect_snapshot(rray_cumulative_any(array(1, c(2L, 2L)), 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(TRUE, class = "foo")
  expect_snapshot(rray_cumulative_all(x, axis = 1L), error = TRUE)
  expect_snapshot(rray_cumulative_any(x, 1L), error = TRUE)
})
