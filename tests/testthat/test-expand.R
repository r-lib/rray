context("test-expand")

test_that("various dimension expanding variations work", {

  x <- rray(1:10, c(5, 2))

  expect_equal(
    rray_dim(rray_expand(x, 1)),
    c(1, 5, 2)
  )

  expect_equal(
    rray_dim(rray_expand(x, 2)),
    c(5, 1, 2)
  )

  expect_equal(
    rray_dim(rray_expand(x, 3)),
    c(5, 2, 1)
  )

})

test_that("error on bad expansion", {

  x <- rray(1:10, c(5, 2))

  expect_error(
    rray_expand(x, 4),
    "The maximum value for `axis` is 3."
  )

})

test_that("dimension names kept on expansion", {

  x <- rray(1:10, c(5, 2))
  x <- rray_set_row_names(x, letters[1:5])
  x <- rray_set_col_names(x, c("c1", "c2"))

  expect_equal(
    rray_dim_names(rray_expand(x, 1)),
    c(list(NULL), rray_dim_names(x))
  )

  expect_equal(
    rray_dim_names(rray_expand(x, 2)),
    c(rray_dim_names(x)[1], list(NULL), rray_dim_names(x)[2])
  )

  expect_equal(
    rray_dim_names(rray_expand(x, 3)),
    c(rray_dim_names(x), list(NULL))
  )

})

test_that("can expand on `NULL` input", {
  expect_equal(rray_expand(NULL, 2), NULL)
})
