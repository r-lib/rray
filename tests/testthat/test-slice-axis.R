# ------------------------------------------------------------------------------
# rray_slice_axis()

test_that("matches `rray_slice()` with `TRUE` on every other axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice_axis(x, c(2L, 1L), axis = 1),
    rray_slice(x, c(2L, 1L), TRUE, TRUE)
  )
  expect_identical(
    rray_slice_axis(x, c(3L, 1L), axis = 2),
    rray_slice(x, TRUE, c(3L, 1L), TRUE)
  )
  expect_identical(
    rray_slice_axis(x, c(4L, 1L), axis = 3),
    rray_slice(x, TRUE, TRUE, c(4L, 1L))
  )
})

test_that("follows the rules of every kind of subscript", {
  x <- array(1:24, c(2L, 3L, 4L), dimnames = list(NULL, c("a", "b", "c"), NULL))
  subscripts <- list(
    c(3L, 1L, 3L),
    c(3, 0, 1),
    -2L,
    c(TRUE, FALSE, TRUE),
    TRUE,
    FALSE,
    NA,
    c(NA, 2L),
    c("c", "a"),
    NULL,
    array(c(3L, 1L), 2L)
  )

  for (i in subscripts) {
    expect_identical(
      rray_slice_axis(x, i, axis = 2),
      rray_slice(x, TRUE, i, TRUE)
    )
  }
})

test_that("only changes the dimension of `axis`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_dimensions(rray_slice_axis(x, c(1L, 1L, 1L, 1L, 1L), axis = 2)),
    c(2L, 5L, 4L)
  )
  expect_identical(
    rray_dimensions(rray_slice_axis(x, NULL, axis = 3)),
    c(2L, 3L, 0L)
  )
})

test_that("selects names of `axis` and keeps names of every other axis", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("c", "d", "e"))
  )

  expect_identical(
    rray_names(rray_slice_axis(x, c(3L, NA, 1L), axis = 2)),
    list(c("a", "b"), c("e", "", "c"))
  )
})

test_that("works with a bare vector", {
  expect_identical(
    rray_slice_axis(c(a = 1L, b = 2L), c(2L, 1L), axis = 1),
    array(c(2L, 1L), 2L, dimnames = list(c("b", "a")))
  )
})

test_that("works with the maximum dimensionality", {
  x <- array(1:2, c(rep(1L, 49L), 2L))

  expect_identical(
    rray_slice_axis(x, 2L, axis = 50),
    array(2L, rep(1L, 50L))
  )
})

test_that("reports subscript errors as `i`", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))

  expect_snapshot(error = TRUE, {
    rray_slice_axis(x, 4L, axis = 2)
    rray_slice_axis(x, "z", axis = 1)
    rray_slice_axis(x, "c", axis = 2)
    rray_slice_axis(x, c(TRUE, FALSE, TRUE), axis = 1)
    rray_slice_axis(x, matrix(1L), axis = 1)
    rray_slice_axis(x, list(1L), axis = 1)
  })
})

test_that("`axis` is validated", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_slice_axis(x, 1L, axis = 0)
    rray_slice_axis(x, 1L, axis = 3)
    rray_slice_axis(x, 1L, axis = c(1, 2))
    rray_slice_axis(x, 1L, axis = NA_integer_)
  })
})

test_that("errors on unsupported `x` inputs", {
  expect_snapshot(error = TRUE, {
    rray_slice_axis(NULL, 1L, axis = 1)
    rray_slice_axis(structure(1:2, class = "foo"), 1L, axis = 1)
  })
})

# ------------------------------------------------------------------------------
# rray_slice_rows()

test_that("is `rray_slice_axis()` with `axis = 1`", {
  x <- array(1:24, c(2L, 3L, 4L), dimnames = list(c("a", "b"), NULL, NULL))

  expect_identical(
    rray_slice_rows(x, c("b", "a")),
    rray_slice_axis(x, c("b", "a"), axis = 1)
  )
})

test_that("reports errors from `rray_slice_rows()`", {
  expect_snapshot(error = TRUE, {
    rray_slice_rows(array(1:6, c(2L, 3L)), 3L)
  })
})

# ------------------------------------------------------------------------------
# rray_slice_columns()

test_that("is `rray_slice_axis()` with `axis = 2`", {
  x <- array(1:24, c(2L, 3L, 4L), dimnames = list(NULL, c("a", "b", "c"), NULL))

  expect_identical(
    rray_slice_columns(x, c("c", "a")),
    rray_slice_axis(x, c("c", "a"), axis = 2)
  )
})

test_that("errors if `x` doesn't have a second axis", {
  expect_snapshot(error = TRUE, {
    rray_slice_columns(1:3, 1L)
  })
})

# ------------------------------------------------------------------------------
# rray_slice_assign_axis()

test_that("matches `rray_slice_assign()` with `TRUE` on every other axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice_assign_axis(x, 2L, axis = 1, value = 0L),
    rray_slice_assign(x, 2L, TRUE, TRUE, value = 0L)
  )
  expect_identical(
    rray_slice_assign_axis(x, c(3L, 1L), axis = 2, value = 0L),
    rray_slice_assign(x, TRUE, c(3L, 1L), TRUE, value = 0L)
  )
  expect_identical(
    rray_slice_assign_axis(x, -1L, axis = 3, value = 0L),
    rray_slice_assign(x, TRUE, TRUE, -1L, value = 0L)
  )
})

test_that("broadcasts `value` to the dimensions of the selection", {
  x <- array(1:6, c(2L, 3L))
  value <- array(c(100L, 200L), c(2L, 1L))

  expect_identical(
    rray_slice_assign_axis(x, c(1L, 3L), axis = 2, value = value),
    array(c(100L, 200L, 3L, 4L, 100L, 200L), c(2L, 3L))
  )
})

test_that("keeps the names of `x`", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))

  expect_identical(
    rray_names(rray_slice_assign_axis(x, "d", axis = 2, value = 0L)),
    rray_names(x)
  )
})

test_that("reports subscript errors from `rray_slice_assign_axis()` as `i`", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))

  expect_snapshot(error = TRUE, {
    rray_slice_assign_axis(x, 4L, axis = 2, value = 0L)
    rray_slice_assign_axis(x, "z", axis = 1, value = 0L)
  })
})

test_that("`axis` and `value` are validated by `rray_slice_assign_axis()`", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_slice_assign_axis(x, 1L, axis = 3, value = 0L)
    rray_slice_assign_axis(x, 1L, axis = 1, value = "a")
    rray_slice_assign_axis(x, 1L, axis = 1, value = 1:2)
  })
})

test_that("errors on unsupported `x` inputs to `rray_slice_assign_axis()`", {
  expect_snapshot(error = TRUE, {
    rray_slice_assign_axis(NULL, 1L, axis = 1, value = 0L)
    rray_slice_assign_axis(
      structure(1:2, class = "foo"),
      1L,
      axis = 1,
      value = 0L
    )
  })
})

# ------------------------------------------------------------------------------
# rray_slice_assign_rows()

test_that("is `rray_slice_assign_axis()` with `axis = 1`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice_assign_rows(x, 2L, 0L),
    rray_slice_assign_axis(x, 2L, axis = 1, value = 0L)
  )
})

test_that("reports errors from `rray_slice_assign_rows()`", {
  expect_snapshot(error = TRUE, {
    rray_slice_assign_rows(array(1:6, c(2L, 3L)), 3L, 0L)
  })
})

# ------------------------------------------------------------------------------
# rray_slice_assign_columns()

test_that("is `rray_slice_assign_axis()` with `axis = 2`", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice_assign_columns(x, c(3L, 1L), 0L),
    rray_slice_assign_axis(x, c(3L, 1L), axis = 2, value = 0L)
  )
})

test_that("`rray_slice_assign_columns()` errors without a second axis", {
  expect_snapshot(error = TRUE, {
    rray_slice_assign_columns(1:3, 1L, 0L)
  })
})
