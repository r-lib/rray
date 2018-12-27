context("test-expand-dims")

test_that("various dimension expanding variations work", {

  x <- rray(1:10, c(5, 2))

  expect_equal(
    vec_dim(rray_expand_dims(x, 1)),
    c(1, 5, 2)
  )

  expect_equal(
    vec_dim(rray_expand_dims(x, 2)),
    c(5, 1, 2)
  )

  expect_equal(
    vec_dim(rray_expand_dims(x, 3)),
    c(5, 2, 1)
  )

})

test_that("error on bad expansion", {

  x <- rray(1:10, c(5, 2))

  expect_error(
    rray_expand_dims(x, 4),
    "`axis` for this `x` can be at most 3, not 4."
  )

})

test_that("dimension names kept on expansion", {

  x <- rray(1:10, c(5, 2))
  x <- set_row_names(x, letters[1:5])
  x <- set_col_names(x, c("c1", "c2"))

  expect_equal(
    dim_names(rray_expand_dims(x, 1)),
    c(list(character()), dim_names(x))
  )

  expect_equal(
    dim_names(rray_expand_dims(x, 2)),
    c(dim_names(x)[1], list(character()), dim_names(x)[2])
  )

  expect_equal(
    dim_names(rray_expand_dims(x, 3)),
    c(dim_names(x), list(character()))
  )

})
