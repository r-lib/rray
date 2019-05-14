context("test-unique")

test_that("uniqueness along `axis = 1` is equal to vctrs", {
  x <- c(1, 1, 2, 2, 3)
  expect_equal(rray_unique(x), vec_unique(x))
  expect_equal(rray_unique_loc(x), vec_unique_loc(x))
  expect_equal(rray_unique_count(x), vec_unique_count(x))

  x <- rray(c(1, 1, 2, 2), c(2, 2))
  expect_equal(rray_unique(x), vec_unique(x))
  expect_equal(rray_unique_loc(x), vec_unique_loc(x))
  expect_equal(rray_unique_count(x), vec_unique_count(x))
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
  x <- set_row_names(x, c("r1"))
  x <- set_col_names(x, c("c1", "c2", "c3", "c4"))

  expect_equal(
    rray_unique(x, axis = 2L),
    x[, c(1, 3)]
  )

  xx <- rray_expand_dims(x, 2)

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

test_that("`NULL` input", {
  expect_equal(rray_unique(NULL, 2L), NULL)
  expect_equal(rray_unique_loc(NULL, 2L), integer(0))
  expect_equal(rray_unique_count(NULL, 2L), 0L)
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
