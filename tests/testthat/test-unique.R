context("test-unique")

test_that("uniqueness along `axis = 1` is equal to vctrs for 1D / 2D", {
  x <- new_array(c(1, 1, 2, 2, 3))
  expect_equal(rray_unique(x, axis = 1), vec_unique(x))
  expect_equal(rray_unique_loc(x, axis = 1), vec_unique_loc(x))
  expect_equal(rray_unique_count(x, axis = 1), vec_unique_count(x))

  x <- rray(c(1, 1, 2, 2), c(2, 2))
  expect_equal(rray_unique(x, axis = 1), vec_unique(x))
  expect_equal(rray_unique_loc(x, axis = 1), vec_unique_loc(x))
  expect_equal(rray_unique_count(x, axis = 1), vec_unique_count(x))
})

test_that("can compute uniqueness along columns", {

  x <- rray(c(1, 1, 2, 2), c(1, 4))

  expect_equal(
    rray_unique(x, axis = 2L),
    rray(c(1, 2), c(1, 2))
  )

  expect_equal(
    rray_unique_loc(x, 2),
    c(1, 3)
  )

  expect_equal(
    rray_unique_loc(x, 1),
    1
  )

  expect_equal(
    rray_unique_count(x, 2),
    2
  )

})

test_that("names are retained", {

  x <- rray(c(1, 1, 2, 2), c(1, 4))
  x <- rray_set_row_names(x, c("r1"))
  x <- rray_set_col_names(x, c("c1", "c2", "c3", "c4"))

  expect_equal(
    rray_unique(x, axis = 2L),
    x[, c(1, 3)]
  )

  xx <- rray_expand(x, 2)

  expect_equal(
    rray_unique(xx, 3L),
    xx[, , c(1, 3)]
  )

  expect_equal(
    rray_unique_loc(xx, axis = 3L),
    c(1, 3)
  )

  expect_equal(
    rray_unique_count(xx, axis = 3L),
    2
  )

})

test_that("`axis` is validated", {
  axis <- c(1, 2)
  expect_error(rray_unique(1, axis), "Invalid `axis`")
  expect_error(rray_unique_loc(1, axis), "Invalid `axis`")
  expect_error(rray_unique_count(1, axis), "Invalid `axis`")

  axis <- -1
  expect_error(rray_unique(1, axis), "Invalid `axis`")
  expect_error(rray_unique_loc(1, axis), "Invalid `axis`")
  expect_error(rray_unique_count(1, axis), "Invalid `axis`")

  axis <- 2
  expect_error(rray_unique(1, axis), "Invalid `axis`")
  expect_error(rray_unique_loc(1, axis), "Invalid `axis`")
  expect_error(rray_unique_count(1, axis), "Invalid `axis`")
})

test_that("rray_unique() is correctly defined over higher dimensions", {
  x_dup_rows <- rray(c(1, 1, 3, 3, 2, 2, 4, 4), c(2, 2, 2))
  x_dup_rows <- rray_set_row_names(x_dup_rows, c("r1", "r2"))
  x_dup_rows <- rray_set_col_names(x_dup_rows, c("c1", "c2"))

  expect_equal(rray_unique(x_dup_rows, 1), x_dup_rows[1,])
  expect_equal(rray_unique(x_dup_rows, 2), x_dup_rows)

  x_dup_cols <- rray_transpose(x_dup_rows, c(2, 1, 3))
  expect_equal(rray_unique(x_dup_cols, 1), x_dup_cols)
  expect_equal(rray_unique(x_dup_cols, 2), x_dup_cols[,1])

  x_dup_layers <- rray_transpose(x_dup_rows, c(2, 3, 1))
  expect_equal(rray_unique(x_dup_layers, 1), x_dup_layers)
  expect_equal(rray_unique(x_dup_layers, 3), x_dup_layers[,,1])
})

test_that("rray_unique_loc() is correctly defined over higher dimensions", {
  x_dup_rows <- rray(c(1, 1, 3, 3, 2, 2, 4, 4), c(2, 2, 2))

  expect_identical(rray_unique_loc(x_dup_rows, 1), 1L)
  expect_identical(rray_unique_loc(x_dup_rows, 2), 1:2)

  x_dup_cols <- rray_transpose(x_dup_rows, c(2, 1, 3))
  expect_identical(rray_unique_loc(x_dup_cols, 1), 1:2)
  expect_identical(rray_unique_loc(x_dup_cols, 2), 1L)

  x_dup_layers <- rray_transpose(x_dup_rows, c(2, 3, 1))
  expect_identical(rray_unique_loc(x_dup_layers, 1), 1:2)
  expect_identical(rray_unique_loc(x_dup_layers, 3), 1L)
})

test_that("rray_unique_count() is correctly defined over higher dimensions", {
  x_dup_rows <- rray(c(1, 1, 3, 3, 2, 2, 4, 4), c(2, 2, 2))

  expect_identical(rray_unique_count(x_dup_rows, 1), 1L)
  expect_identical(rray_unique_count(x_dup_rows, 2), 2L)

  x_dup_cols <- rray_transpose(x_dup_rows, c(2, 1, 3))
  expect_identical(rray_unique_count(x_dup_cols, 1), 2L)
  expect_identical(rray_unique_count(x_dup_cols, 2), 1L)

  x_dup_layers <- rray_transpose(x_dup_rows, c(2, 3, 1))
  expect_identical(rray_unique_count(x_dup_layers, 1), 2L)
  expect_identical(rray_unique_count(x_dup_layers, 3), 1L)
})

# ------------------------------------------------------------------------------
context("test-base-unique")

test_that("results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5))
  x_base <- vec_data(x)

  expect_equal(unique(x), as_rray(unique(x_base)))
})

test_that("matrix/array results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(unique(x), as_rray(unique(x_base)))

  expect_equal(
    unique(x, MARGIN = 2),
    as_rray(unique(x_base, MARGIN = 2))
  )
})

test_that("cannot use multiple margin", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  expect_error(unique(x, MARGIN = c(1, 2)))
})

test_that("cannot use margin of 0", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  expect_error(unique(x, MARGIN = 0))
})

test_that("incomparables is an error from base R", {
  expect_error(unique(rray(1), incomparables = TRUE))
})

test_that("dim names are kept with base R rules", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3), dim_names = list(letters[1:2], letters[3:5]))
  x_base <- vec_data(x)

  expect_equal(
    rray_dim_names(unique(x)),
    rray_dim_names(unique(x_base))
  )

  expect_equal(
    rray_dim_names(unique(x, MARGIN = 2)),
    rray_dim_names(unique(x_base, MARGIN = 2))
  )
})

test_that("fromLast works", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(
    unique(x, MARGIN = 2, fromLast = TRUE),
    as_rray(unique(x_base, MARGIN = 2, fromLast = TRUE))
  )
})
