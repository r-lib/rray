x <- rray(1:10)
y <- rray_reshape(x, c(5, 2))

# ------------------------------------------------------------------------------
# sum

context("test-reducer-sum")

test_that("Results are correct", {
  expect_equal(as.vector(rray_sum(y, 1)), c(15, 40))
  expect_equal(as.vector(rray_sum(y, 2)), c(7, 9, 11, 13, 15))
})

test_that("Default `axes` argument reduces over all dimensions", {
  expect_equal(
    as.vector(rray_sum(y)),
    sum(vec_data(y))
  )

  expect_equal(
    rray_dim_n(rray_sum(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(rray_dim(rray_sum(y, 1)), c(1, 2))
  expect_equal(rray_dim(rray_sum(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- rray_set_col_names(y, c("c1", "c2"))
  yy <- rray_set_row_names(yy, letters[1:5])

  expect_equal(rray_col_names(rray_sum(yy, 1)), c("c1", "c2"))
  expect_equal(rray_row_names(rray_sum(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_sum(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(as.vector(out), rep(6, times = 4))
  expect_equal(rray_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_sum(x, 1)
  ..2D <- rray_sum(x_mat, c(1, 2))

  expect_equal(rray_dim(..1D), 1L)
  expect_equal(rray_dim(..2D), c(1L, 1L))

  expect_equal(as.vector(..1D), 55)
  expect_equal(as.vector(..2D), 55)
})

test_that("reducing base types maintains type", {

  x_int <- vec_data(x)

  x_mat <- as.matrix(x)
  x_mat_cnames <- rray_set_col_names(x_mat, "x")

  x_arr <- as.array(x)

  expect_is(rray_sum(x_mat, 1), "matrix")
  expect_equal(storage.mode(rray_sum(x_arr, 1)), "double")

  expect_equal(rray_dim(rray_sum(x_mat, 1)), c(1, 1))

  expect_equal(rray_col_names(rray_sum(x_mat_cnames, 1)), "x")
  expect_equal(rray_col_names(rray_sum(x_mat_cnames, 2)), "x")
})

test_that("rray_sum() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_ptype(rray_sum(rray(1L))),
    vec_ptype(rray(1))
  )
})

test_that("can reduce over multiple axes if one is length 0", {
  x <- matrix(numeric(), 0, 2)
  expect_equal(rray_sum(x), new_matrix(0, c(1, 1)))
  expect_equal(rray_sum(x, c(1, 2)), new_matrix(0, c(1, 1)))
})

test_that("reducing with length 0 axis produces the correct result dim", {
  x <- matrix(numeric(), 0, 2)

  # (0, 2) -> (1, 2) b/c reducers always "reduce" `axis` to length 1
  expect_equal(rray_sum(x, 1L), new_matrix(0, c(1, 2)))

  # (0, 2) -> (0, 1)
  expect_equal(rray_sum(x, 2L), new_matrix(numeric(), c(0, 1)))
})

# ------------------------------------------------------------------------------
# prod

context("test-reducer-prod")

test_that("Results are correct", {

  expect_equal(
    as.vector(rray_prod(y, 1)),
    vapply(seq_len(ncol(y)), function(i) prod(as.vector(y[,i])), numeric(1))
  )

  expect_equal(
    as.vector(rray_prod(y, 2)),
    vapply(seq_len(nrow(y)), function(i) prod(as.vector(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dimensions", {
  expect_equal(
    as.vector(rray_prod(y)),
    prod(vec_data(y))
  )

  expect_equal(
    rray_dim_n(rray_prod(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(rray_dim(rray_prod(y, 1)), c(1, 2))
  expect_equal(rray_dim(rray_prod(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- rray_set_col_names(y, c("c1", "c2"))
  yy <- rray_set_row_names(yy, letters[1:5])

  expect_equal(rray_col_names(rray_prod(yy, 1)), c("c1", "c2"))
  expect_equal(rray_row_names(rray_prod(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_prod(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(as.vector(out), rep(1, times = 4))
  expect_equal(rray_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_prod(x, 1)
  ..2D <- rray_prod(x_mat, c(1, 2))

  expect_equal(rray_dim(..1D), 1L)
  expect_equal(rray_dim(..2D), c(1L, 1L))

  expect_equal(as.vector(..1D), prod(vec_data(x)))
  expect_equal(as.vector(..2D), prod(vec_data(x)))
})

test_that("reducing base types maintains type", {

  x_int <- vec_data(x)

  x_mat <- as.matrix(x)
  x_mat_cnames <- rray_set_col_names(x_mat, "x")

  x_arr <- as.array(x)

  expect_is(rray_prod(x_mat, 1), "matrix")
  expect_equal(storage.mode(rray_prod(x_arr, 1)), "double")

  expect_equal(rray_dim(rray_prod(x_mat, 1)), c(1, 1))

  expect_equal(rray_col_names(rray_prod(x_mat_cnames, 1)), "x")
  expect_equal(rray_col_names(rray_prod(x_mat_cnames, 2)), "x")
})

test_that("rray_prod() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_ptype(rray_prod(rray(1L))),
    vec_ptype(rray(1))
  )
})

test_that("can reduce over multiple axes if one is length 0", {
  x <- matrix(numeric(), 0, 2)
  expect_equal(rray_prod(x), new_matrix(1, c(1, 1)))
  expect_equal(rray_prod(x, c(1, 2)), new_matrix(1, c(1, 1)))
})

test_that("reducing with length 0 axis produces the correct result dim", {
  x <- matrix(numeric(), 0, 2)

  # (0, 2) -> (1, 2) b/c reducers always "reduce" `axis` to length 1
  expect_equal(rray_prod(x, 1L), new_matrix(1, c(1, 2)))

  # (0, 2) -> (0, 1)
  expect_equal(rray_prod(x, 2L), new_matrix(numeric(), c(0, 1)))
})

# ------------------------------------------------------------------------------
# mean

context("test-reducer-mean")

test_that("Results are correct", {

  expect_equal(
    as.vector(rray_mean(y, 1)),
    vapply(seq_len(ncol(y)), function(i) mean(as.vector(y[,i])), numeric(1))
  )

  expect_equal(
    as.vector(rray_mean(y, 2)),
    vapply(seq_len(nrow(y)), function(i) mean(as.vector(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dimensions", {
  expect_equal(
    as.vector(rray_mean(y)),
    mean(vec_data(y))
  )

  expect_equal(
    rray_dim_n(rray_mean(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(rray_dim(rray_mean(y, 1)), c(1, 2))
  expect_equal(rray_dim(rray_mean(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- rray_set_col_names(y, c("c1", "c2"))
  yy <- rray_set_row_names(yy, letters[1:5])

  expect_equal(rray_col_names(rray_mean(yy, 1)), c("c1", "c2"))
  expect_equal(rray_row_names(rray_mean(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_mean(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(as.vector(out), rep(1, times = 4))
  expect_equal(rray_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_mean(x, 1)
  ..2D <- rray_mean(x_mat, c(1, 2))

  expect_equal(rray_dim(..1D), 1L)
  expect_equal(rray_dim(..2D), c(1L, 1L))

  expect_equal(as.vector(..1D), mean(vec_data(x)))
  expect_equal(as.vector(..2D), mean(vec_data(x)))
})

test_that("reducing base types maintains type", {

  x_int <- vec_data(x)

  x_mat <- as.matrix(x)
  x_mat_cnames <- rray_set_col_names(x_mat, "x")

  x_arr <- as.array(x)

  expect_is(rray_mean(x_mat, 1), "matrix")
  expect_equal(storage.mode(rray_mean(x_arr, 1)), "double")

  expect_equal(rray_dim(rray_mean(x_mat, 1)), c(1, 1))

  expect_equal(rray_col_names(rray_mean(x_mat_cnames, 1)), "x")
  expect_equal(rray_col_names(rray_mean(x_mat_cnames, 2)), "x")
})

test_that("rray_mean() with integers gives doubles to prevent overflow", {
  expect_equal(
    vec_ptype(rray_mean(rray(1L))),
    vec_ptype(rray(1))
  )
})

test_that("can reduce over multiple axes if one is length 0", {
  x <- matrix(numeric(), 0, 2)
  expect_equal(rray_mean(x), new_matrix(NaN, c(1, 1)))
  expect_equal(rray_mean(x, c(1, 2)), new_matrix(NaN, c(1, 1)))
})

test_that("reducing with length 0 axis produces the correct result dim", {
  x <- matrix(numeric(), 0, 2)

  # (0, 2) -> (1, 2) b/c reducers always "reduce" `axis` to length 1
  expect_equal(rray_mean(x, 1L), new_matrix(NaN, c(1, 2)))

  # (0, 2) -> (0, 1)
  #expect_equal(rray_mean(x, 2L), new_matrix(numeric(), c(0, 1)))
})

# ------------------------------------------------------------------------------
# amax

context("test-reducer-amax")

test_that("Results are correct", {

  expect_equal(
    as.vector(rray_max(y, 1)),
    vapply(seq_len(ncol(y)), function(i) max(as.vector(y[,i])), numeric(1))
  )

  expect_equal(
    as.vector(rray_max(y, 2)),
    vapply(seq_len(nrow(y)), function(i) max(as.vector(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dimensions", {
  expect_equal(
    as.vector(rray_max(y)),
    max(vec_data(y))
  )

  expect_equal(
    rray_dim_n(rray_max(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(rray_dim(rray_max(y, 1)), c(1, 2))
  expect_equal(rray_dim(rray_max(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- rray_set_col_names(y, c("c1", "c2"))
  yy <- rray_set_row_names(yy, letters[1:5])

  expect_equal(rray_col_names(rray_max(yy, 1)), c("c1", "c2"))
  expect_equal(rray_row_names(rray_max(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_max(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(as.vector(out), rep(1, times = 4))
  expect_equal(rray_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_max(x, 1)
  ..2D <- rray_max(x_mat, c(1, 2))

  expect_equal(rray_dim(..1D), 1L)
  expect_equal(rray_dim(..2D), c(1L, 1L))

  expect_equal(as.vector(..1D), max(vec_data(x)))
  expect_equal(as.vector(..2D), max(vec_data(x)))
})

test_that("reducing base types maintains type", {

  x_int <- vec_data(x)

  x_mat <- as.matrix(x)
  x_mat_cnames <- rray_set_col_names(x_mat, "x")

  x_arr <- as.array(x)

  expect_is(rray_max(x_mat, 1), "matrix")
  expect_equal(storage.mode(rray_max(x_arr, 1)), "integer")


  expect_equal(rray_dim(rray_max(x_mat, 1)), c(1, 1))

  expect_equal(rray_col_names(rray_max(x_mat_cnames, 1)), "x")
  expect_equal(rray_col_names(rray_max(x_mat_cnames, 2)), "x")
})

test_that("can reduce over multiple axes if one is length 0", {
  x <- matrix(numeric(), 0, 2)
  expect_equal(rray_max(x), new_matrix(-Inf, c(1, 1)))
  expect_equal(rray_max(x, c(1, 2)), new_matrix(-Inf, c(1, 1)))
})

test_that("reducing with length 0 axis produces the correct result dim", {
  x <- matrix(numeric(), 0, 2)

  # (0, 2) -> (1, 2) b/c reducers always "reduce" `axis` to length 1
  expect_equal(rray_max(x, 1L), new_matrix(-Inf, c(1, 2)))

  # (0, 2) -> (0, 1)
  expect_equal(rray_max(x, 2L), new_matrix(numeric(), c(0, 1)))
})

# ------------------------------------------------------------------------------
# amin

context("test-reducer-amin")

test_that("Results are correct", {

  expect_equal(
    as.vector(rray_min(y, 1)),
    vapply(seq_len(ncol(y)), function(i) min(as.vector(y[,i])), numeric(1))
  )

  expect_equal(
    as.vector(rray_min(y, 2)),
    vapply(seq_len(nrow(y)), function(i) min(as.vector(y[i,])), numeric(1))
  )
})

test_that("Default `axes` argument reduces over all dimensions", {
  expect_equal(
    as.vector(rray_min(y)),
    min(vec_data(y))
  )

  expect_equal(
    rray_dim_n(rray_min(y)),
    2L
  )
})

test_that("Dimensions are not dropped (by default)", {
  expect_equal(rray_dim(rray_min(y, 1)), c(1, 2))
  expect_equal(rray_dim(rray_min(y, 2)), c(5, 1))
})

test_that("Dimension names are kept", {

  yy <- rray_set_col_names(y, c("c1", "c2"))
  yy <- rray_set_row_names(yy, letters[1:5])

  expect_equal(rray_col_names(rray_min(yy, 1)), c("c1", "c2"))
  expect_equal(rray_row_names(rray_min(yy, 2)), letters[1:5])
})

test_that("Can reduce over multiple axes", {
  out <- rray_min(rray(1, c(2, 3, 4)), c(1,2))
  expect_equal(as.vector(out), rep(1, times = 4))
  expect_equal(rray_dim(out), c(1, 1, 4))
})

test_that("Reducing to 0D works", {

  x_mat <- as.matrix(x)

  ..1D <- rray_min(x, 1)
  ..2D <- rray_min(x_mat, c(1, 2))

  expect_equal(rray_dim(..1D), 1L)
  expect_equal(rray_dim(..2D), c(1L, 1L))

  expect_equal(as.vector(..1D), min(vec_data(x)))
  expect_equal(as.vector(..2D), min(vec_data(x)))
})

test_that("reducing base types maintains type", {

  x_int <- vec_data(x)

  x_mat <- as.matrix(x)
  x_mat_cnames <- rray_set_col_names(x_mat, "x")

  x_arr <- as.array(x)

  expect_is(rray_min(x_mat, 1), "matrix")
  expect_equal(storage.mode(rray_min(x_arr, 1)), "integer")

  expect_equal(rray_dim(rray_min(x_mat, 1)), c(1, 1))

  expect_equal(rray_col_names(rray_min(x_mat_cnames, 1)), "x")
  expect_equal(rray_col_names(rray_min(x_mat_cnames, 2)), "x")
})

test_that("can reduce over multiple axes if one is length 0", {
  x <- matrix(numeric(), 0, 2)
  expect_equal(rray_min(x), new_matrix(Inf, c(1, 1)))
  expect_equal(rray_min(x, c(1, 2)), new_matrix(Inf, c(1, 1)))
})

test_that("reducing with length 0 axis produces the correct result dim", {
  x <- matrix(numeric(), 0, 2)

  # (0, 2) -> (1, 2) b/c reducers always "reduce" `axis` to length 1
  expect_equal(rray_min(x, 1L), new_matrix(Inf, c(1, 2)))

  # (0, 2) -> (0, 1)
  expect_equal(rray_min(x, 2L), new_matrix(numeric(), c(0, 1)))
})

# ------------------------------------------------------------------------------
# Scalar reductions

test_that("reductions with scalars don't crash R", {
  expect_equal(
    rray_sum(1, axes = NULL),
    rray_sum(1, axes = 1L)
  )
})

