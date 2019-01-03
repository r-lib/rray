x <- 1:10
x <- rlang::set_names(x, letters[1:10])

x_array <- as.array(x)

x_mat <- as.matrix(x)

# ------------------------------------------------------------------------------
context("test-coercion-matrix")

test_that("Can coerce to matrices from various inputs", {
  expect_is(as_matrix(x), "matrix")
  expect_is(as_matrix(x_array), "matrix")
  expect_is(as_matrix(x_mat), "matrix")
})

test_that("vector -> matrix makes a 1 column matrix", {
  expect_equal(vec_dim(as_matrix(x)), c(vec_size(x), 1L))
})

test_that("names() and dim_names() are kept", {

  nms <- c(dim_names(x), new_empty_dim_names(1))

  expect_equal(dim_names(as_matrix(x)), nms)
  expect_equal(dim_names(as_matrix(x_array)), nms)
  expect_equal(dim_names(as_matrix(x_mat)), nms)
})

test_that("meta dim names are kept", {

  x_nms <- dim_names(x_array)
  names(x_nms) <- "meta_nm"
  x_array_meta <- x_array
  dim_names(x_array_meta) <- x_nms

  nms_with_meta <- c(dim_names(x_array_meta), new_empty_dim_names(1))

  expect_equal(
    dim_names(as_matrix(x_array_meta)),
    nms_with_meta
  )
})

test_that("Cannot reduce dimensions", {
  expect_error(as_matrix(rray(1, c(1, 1, 1))), "Cannot reduce")
})

# ------------------------------------------------------------------------------
context("test-coercion-array")

test_that("Can coerce to arrays from various inputs", {
  expect_is(as_array(x), "array")
  expect_is(as_array(x_array), "array")
  # 2D objects are matrices
  expect_is(as_array(x_mat), "matrix")
})

test_that("vector -> array makes a 1D array", {
  expect_equal(vec_dim(as_array(x)), vec_size(x))
})

test_that("names() and dim_names() are kept", {

  nms_1D <- dim_names(x)
  nms_2D <- c(dim_names(x), new_empty_dim_names(1))

  expect_equal(dim_names(as_array(x)), nms_1D)
  expect_equal(dim_names(as_array(x_array)), nms_1D)
  expect_equal(dim_names(as_array(x_mat)), nms_2D)
})

test_that("meta dim names are kept", {

  x_rray <- as_rray(x_array)
  x_nms <- dim_names(x_rray)
  names(x_nms) <- "meta_nm"
  dim_names(x_rray) <- x_nms

  nms_with_meta <- dim_names(x_rray)

  expect_equal(
    dim_names(as_array(x_rray)),
    nms_with_meta
  )
})

# ------------------------------------------------------------------------------
context("test-coercion-rray")

test_that("Can coerce to rrays from various inputs", {
  expect_is(as_rray(x), "vctrs_rray")
  expect_is(as_rray(x_array), "vctrs_rray")
  expect_is(as_rray(x_mat), "vctrs_rray")
})

test_that("vector -> rray makes a 1D rray", {
  expect_equal(vec_dim(as_rray(x)), vec_size(x))
})

test_that("names() and dim_names() are kept", {

  nms_1D <- dim_names(x)
  nms_2D <- c(dim_names(x), new_empty_dim_names(1))

  expect_equal(dim_names(as_rray(x)), nms_1D)
  expect_equal(dim_names(as_rray(x_array)), nms_1D)
  expect_equal(dim_names(as_rray(x_mat)), nms_2D)
})

test_that("meta dim names are kept", {

  x_nms <- dim_names(x_array)
  names(x_nms) <- "meta_nm"
  x_array_meta <- x_array
  dim_names(x_array_meta) <- x_nms

  nms_with_meta <- dim_names(x_array_meta)

  expect_equal(
    dim_names(as_rray(x_array_meta)),
    nms_with_meta
  )
})

test_that("4D tests", {

  x_4D <- array(1, c(1, 1, 1, 1), dimnames = list("r1", "c1", "..3_1", "..4_1"))
  nms_4D <- dim_names(x_4D)

  expect_equal(vec_dim(as_rray(x_4D)), c(1, 1, 1, 1))
  expect_equal(dim_names(as_rray(x_4D)), nms_4D)
})
