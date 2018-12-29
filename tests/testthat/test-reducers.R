x <- rray(1:10)
y <- rray_reshape(x, c(5, 2))

# ------------------------------------------------------------------------------
# sum

context("test-reducer-sum")

test_that("Results are correct", {
  expect_equal(vec_data(rray_sum(y, 1)), c(15, 40))
  expect_equal(vec_data(rray_sum(y, 2)), c(7, 9, 11, 13, 15))
})

test_that("Default `axes` argument reduces over all dims", {
  expect_equal(
    vec_data(rray_sum(y)),
    sum(vec_data(y))
  )

  expect_equal(
    vec_dims(rray_sum(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_sum(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_sum(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_sum(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_sum(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_sum(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(6, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_sum(x, 1)
  ..2D <- rray_sum(x_mat, c(1, 2))

  expect_equal(vec_dim(..1D), 1L)
  expect_equal(vec_dim(..2D), c(1L, 1L))

  expect_equal(vec_data(..1D), 55)
  expect_equal(vec_data(..2D), 55)
})

test_that("rray_sum() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_type(rray_sum(rray(1L))),
    vec_type(rray(1))
  )
})

# ------------------------------------------------------------------------------
# prod

context("test-reducer-prod")

test_that("Results are correct", {

  expect_equal(
    vec_data(rray_prod(y, 1)),
    vapply(seq_len(ncol(y)), function(i) prod(vec_data(y[,i])), numeric(1))
  )

  expect_equal(
    vec_data(rray_prod(y, 2)),
    vapply(seq_len(nrow(y)), function(i) prod(vec_data(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dims", {
  expect_equal(
    vec_data(rray_prod(y)),
    prod(vec_data(y))
  )

  expect_equal(
    vec_dims(rray_prod(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_prod(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_prod(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_prod(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_prod(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_prod(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(1, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_prod(x, 1)
  ..2D <- rray_prod(x_mat, c(1, 2))

  expect_equal(vec_dim(..1D), 1L)
  expect_equal(vec_dim(..2D), c(1L, 1L))

  expect_equal(vec_data(..1D), prod(vec_data(x)))
  expect_equal(vec_data(..2D), prod(vec_data(x)))
})

test_that("rray_prod() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_type(rray_prod(rray(1L))),
    vec_type(rray(1))
  )
})


# ------------------------------------------------------------------------------
# mean

context("test-reducer-mean")

test_that("Results are correct", {

  expect_equal(
    vec_data(rray_mean(y, 1)),
    vapply(seq_len(ncol(y)), function(i) mean(vec_data(y[,i])), numeric(1))
  )

  expect_equal(
    vec_data(rray_mean(y, 2)),
    vapply(seq_len(nrow(y)), function(i) mean(vec_data(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dims", {
  expect_equal(
    vec_data(rray_mean(y)),
    mean(vec_data(y))
  )

  expect_equal(
    vec_dims(rray_mean(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_mean(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_mean(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_mean(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_mean(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_mean(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(1, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_mean(x, 1)
  ..2D <- rray_mean(x_mat, c(1, 2))

  expect_equal(vec_dim(..1D), 1L)
  expect_equal(vec_dim(..2D), c(1L, 1L))

  expect_equal(vec_data(..1D), mean(vec_data(x)))
  expect_equal(vec_data(..2D), mean(vec_data(x)))
})

test_that("rray_mean() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_type(rray_mean(rray(1L))),
    vec_type(rray(1))
  )
})

# ------------------------------------------------------------------------------
# amax

context("test-reducer-amax")

test_that("Results are correct", {

  expect_equal(
    vec_data(rray_amax(y, 1)),
    vapply(seq_len(ncol(y)), function(i) max(vec_data(y[,i])), numeric(1))
  )

  expect_equal(
    vec_data(rray_amax(y, 2)),
    vapply(seq_len(nrow(y)), function(i) max(vec_data(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dims", {
  expect_equal(
    vec_data(rray_amax(y)),
    max(vec_data(y))
  )

  expect_equal(
    vec_dims(rray_amax(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_amax(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_amax(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_amax(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_amax(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_amax(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(1, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_amax(x, 1)
  ..2D <- rray_amax(x_mat, c(1, 2))

  expect_equal(vec_dim(..1D), 1L)
  expect_equal(vec_dim(..2D), c(1L, 1L))

  expect_equal(vec_data(..1D), max(vec_data(x)))
  expect_equal(vec_data(..2D), max(vec_data(x)))
})

test_that("rray_amax() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_type(rray_amax(rray(1L))),
    vec_type(rray(1))
  )
})

# ------------------------------------------------------------------------------
# amin

context("test-reducer-amin")

test_that("Results are correct", {

  expect_equal(
    vec_data(rray_amin(y, 1)),
    vapply(seq_len(ncol(y)), function(i) min(vec_data(y[,i])), numeric(1))
  )

  expect_equal(
    vec_data(rray_amin(y, 2)),
    vapply(seq_len(nrow(y)), function(i) min(vec_data(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dims", {
  expect_equal(
    vec_data(rray_amin(y)),
    min(vec_data(y))
  )

  expect_equal(
    vec_dims(rray_amin(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(vec_dim(rray_amin(y, 1)), c(1, 2))
  expect_equal(vec_dim(rray_amin(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- set_col_names(y, c("c1", "c2"))
  yy <- set_row_names(yy, letters[1:5])

  expect_equal(col_names(rray_amin(yy, 1)), c("c1", "c2"))
  expect_equal(row_names(rray_amin(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_amin(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(vec_data(out), rep(1, times = 4))
  expect_equal(vec_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_amin(x, 1)
  ..2D <- rray_amin(x_mat, c(1, 2))

  expect_equal(vec_dim(..1D), 1L)
  expect_equal(vec_dim(..2D), c(1L, 1L))

  expect_equal(vec_data(..1D), min(vec_data(x)))
  expect_equal(vec_data(..2D), min(vec_data(x)))
})

test_that("rray_amin() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_type(rray_amin(rray(1L))),
    vec_type(rray(1))
  )
})
