# This `x` is complex enough that it hits most of the complexity of
# transposing / reshaping

# ------------------------------------------------------------------------------
context("test-duplicate-any")

test_that("along 1 axis works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_any(x, 1)), c(1, 3, 2))
  expect_equal(rray_dim(rray_duplicate_any(x, 2)), c(2, 1, 2))
  expect_equal(rray_dim(rray_duplicate_any(x, 3)), c(2, 3, 1))

  expect_equal(rray_duplicate_any(x, 1), new_array(c(F, F, F, T, F, F), c(1, 3, 2)))
  expect_equal(rray_duplicate_any(x, 2), new_array(c(T, F, T, F),       c(2, 1, 2)))
  expect_equal(rray_duplicate_any(x, 3), new_array(c(F, T, F, F, F, T), c(2, 3, 1)))
})

test_that("along 2 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_any(x, c(1, 2))), c(1, 1, 2))
  expect_equal(rray_dim(rray_duplicate_any(x, c(1, 3))), c(1, 3, 1))
  expect_equal(rray_dim(rray_duplicate_any(x, c(2, 3))), c(2, 1, 1))

  expect_equal(rray_duplicate_any(x, c(1, 2)), new_array(c(T, T),    c(1, 1, 2)))
  expect_equal(rray_duplicate_any(x, c(1, 3)), new_array(c(T, F, T), c(1, 3, 1)))
  expect_equal(rray_duplicate_any(x, c(2, 3)), new_array(c(T, T),    c(2, 1, 1)))
})

test_that("along 3 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_any(x, c(1, 2, 3))), c(1, 1, 1))

  expect_equal(rray_duplicate_any(x, c(1, 2, 3)), new_array(T, c(1, 1, 1)))
})

test_that("`axis` is validated", {
  axis <- -1
  expect_error(rray_duplicate_any(1, axis), "Invalid `axes`")

  axis <- 2
  expect_error(rray_duplicate_any(1, axis), "Invalid `axes`")
})


test_that("`NULL` axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_any(x, axes = NULL),
    rray_duplicate_any(x, axes = seq_len(rray_dim_n(x)))
  )
})

# no reduction is done. treats each cell individually, so there can't be dups
test_that("length 0 integer axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_any(x, axes = integer()),
    new_array(FALSE, c(2, 3, 2))
  )
})

test_that("names are kept", {
  x <- matrix(1, dimnames = list("c1", "c2"))
  expect_equal(rray_dim_names(rray_duplicate_any(x)), rray_dim_names(x))
})

test_that("`axes` must be ordered and unique", {
  expect_error(rray_duplicate_any(matrix(1), c(2, 1)), "in ascending order")
  expect_error(rray_duplicate_any(matrix(1), c(2, 2)), "in ascending order")
})

test_that("rray class is kept", {
  expect_equal(rray_duplicate_any(rray(1), 1), rray(FALSE))
})

# ------------------------------------------------------------------------------
context("test-duplicate-detect")

test_that("along 1 axis works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_detect(x, 1)), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_detect(x, 2)), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_detect(x, 3)), c(2, 3, 2))

  expect_equal(rray_duplicate_detect(x, 1), new_array(c(F, F, F, F, F, F, T, T, F, F, F, F), c(2, 3, 2)))
  expect_equal(rray_duplicate_detect(x, 2), new_array(c(T, F, T, F, F, F, T, F, T, F, F, F), c(2, 3, 2)))
  expect_equal(rray_duplicate_detect(x, 3), new_array(c(F, T, F, F, F, T, F, T, F, F, F, T), c(2, 3, 2)))
})

test_that("along 2 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_detect(x, c(1, 2))), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_detect(x, c(1, 3))), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_detect(x, c(2, 3))), c(2, 3, 2))

  expect_equal(rray_duplicate_detect(x, c(1, 2)), new_array(c(T, F, T, T, T, T, T, T, T, F, F, F), c(2, 3, 2)))
  expect_equal(rray_duplicate_detect(x, c(1, 3)), new_array(c(F, T, F, F, F, T, T, T, F, F, F, T), c(2, 3, 2)))
  expect_equal(rray_duplicate_detect(x, c(2, 3)), new_array(c(T, T, T, F, F, T, T, T, T, F, F, T), c(2, 3, 2)))
})

test_that("along 3 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_detect(x, c(1, 2, 3))), c(2, 3, 2))

  expect_equal(rray_duplicate_detect(x, c(1, 2, 3)), new_array(c(T, T, T, T, T, T, T, T, T, F, F, T), c(2, 3, 2)))
})

test_that("`axis` is validated", {
  axis <- -1
  expect_error(rray_duplicate_detect(1, axis), "Invalid `axes`")

  axis <- 2
  expect_error(rray_duplicate_detect(1, axis), "Invalid `axes`")
})


test_that("`NULL` axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_detect(x, axes = NULL),
    rray_duplicate_detect(x, axes = seq_len(rray_dim_n(x)))
  )
})

# no reduction is done. treats each cell individually, so there can't be dups
test_that("length 0 integer axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_detect(x, axes = integer()),
    new_array(FALSE, c(2, 3, 2))
  )
})

test_that("names are kept", {
  x <- matrix(1, dimnames = list("c1", "c2"))
  expect_equal(rray_dim_names(rray_duplicate_detect(x)), rray_dim_names(x))
})

test_that("`axes` must be ordered and unique", {
  expect_error(rray_duplicate_detect(matrix(1), c(2, 1)), "in ascending order")
  expect_error(rray_duplicate_detect(matrix(1), c(2, 2)), "in ascending order")
})

test_that("rray class is kept", {
  expect_equal(rray_duplicate_detect(rray(1), 1), rray(FALSE))
})

# ------------------------------------------------------------------------------
context("test-duplicate-id")

test_that("along 1 axis works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_id(x, 1)), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_id(x, 2)), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_id(x, 3)), c(2, 3, 2))

  expect_equal(rray_duplicate_id(x, 1), new_array(c(1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2), c(2, 3, 2)))
  expect_equal(rray_duplicate_id(x, 2), new_array(c(1, 1, 1, 2, 3, 3, 1, 1, 1, 2, 3, 3), c(2, 3, 2)))
  expect_equal(rray_duplicate_id(x, 3), new_array(c(1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1), c(2, 3, 2)))
})

test_that("along 2 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_id(x, c(1, 2))), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_id(x, c(1, 3))), c(2, 3, 2))
  expect_equal(rray_dim(rray_duplicate_id(x, c(2, 3))), c(2, 3, 2))

  expect_equal(rray_duplicate_id(x, c(1, 2)), new_array(c(1, 2, 1, 4, 4, 1, 1, 1, 1, 4, 5, 6), c(2, 3, 2)))
  expect_equal(rray_duplicate_id(x, c(1, 3)), new_array(c(1, 2, 1, 2, 1, 2, 2, 2, 3, 4, 3, 2), c(2, 3, 2)))
  expect_equal(rray_duplicate_id(x, c(2, 3)), new_array(c(1, 1, 1, 2, 3, 3, 4, 1, 4, 5, 6, 3), c(2, 3, 2)))
})

test_that("along 3 axes works", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))

  expect_equal(rray_dim(rray_duplicate_id(x, c(1, 2, 3))), c(2, 3, 2))

  expect_equal(rray_duplicate_id(x, c(1, 2, 3)), new_array(c(1, 2, 1, 4, 4, 1, 2, 2, 2, 10, 11, 1), c(2, 3, 2)))
})

test_that("`axis` is validated", {
  axis <- -1
  expect_error(rray_duplicate_id(1, axis), "Invalid `axes`")

  axis <- 2
  expect_error(rray_duplicate_id(1, axis), "Invalid `axes`")
})


test_that("`NULL` axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_id(x, axes = NULL),
    rray_duplicate_id(x, axes = seq_len(rray_dim_n(x)))
  )
})

# no reduction is done. treats each cell individually, so there can't be dups
test_that("length 0 integer axes", {
  x <- array(c(1, 4, 1, 3, 3, 1, 4, 4, 4, 6, 7, 1), c(2, 3, 2))
  expect_equal(
    rray_duplicate_id(x, axes = integer()),
    new_array(1, c(2, 3, 2))
  )
})

test_that("names are kept", {
  x <- matrix(1, dimnames = list("c1", "c2"))
  expect_equal(rray_dim_names(rray_duplicate_id(x)), rray_dim_names(x))
})

test_that("`axes` must be ordered and unique", {
  expect_error(rray_duplicate_id(matrix(1), c(2, 1)), "in ascending order")
  expect_error(rray_duplicate_id(matrix(1), c(2, 2)), "in ascending order")
})

test_that("rray class is kept", {
  expect_equal(rray_duplicate_id(rray(1), 1), rray(1L))
})

# ------------------------------------------------------------------------------
context("test-duplicate-base-duplicated")

test_that("results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5))
  x_base <- vec_data(x)

  expect_equal(duplicated(x), as_rray(duplicated(x_base)))
})

test_that("matrix/array results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(duplicated(x), as_rray(duplicated(x_base)))

  expect_equal(
    duplicated(x, MARGIN = 2),
    as_rray(duplicated(x_base, MARGIN = 2))
  )

  expect_equal(
    duplicated(x, MARGIN = c(1, 2)),
    as_rray(duplicated(x_base, MARGIN = c(1, 2)))
  )

  expect_equal(
    duplicated(x, MARGIN = 0),
    as_rray(duplicated(x_base, MARGIN = 0))
  )
})

test_that("incomparables is an error from base R", {
  expect_error(duplicated(rray(1), incomparables = TRUE))
})

test_that("dim names are kept with base R rules", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3), dim_names = list(letters[1:2], letters[3:5]))
  x_base <- vec_data(x)

  expect_equal(
    rray_dim_names(duplicated(x)),
    rray_dim_names(duplicated(x_base))
  )

  expect_equal(
    rray_dim_names(duplicated(x, MARGIN = c(1, 2))),
    rray_dim_names(duplicated(x_base, MARGIN = c(1, 2)))
  )
})

test_that("fromLast works", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(
    duplicated(x, MARGIN = 2, fromLast = TRUE),
    as_rray(duplicated(x_base, MARGIN = 2, fromLast = TRUE))
  )

  expect_equal(
    duplicated(x, MARGIN = c(1, 2), fromLast = TRUE),
    as_rray(duplicated(x_base, MARGIN = c(1, 2), fromLast = TRUE))
  )
})

# ------------------------------------------------------------------------------
context("test-duplicate-base-anyDuplicated")

test_that("results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5))
  x_base <- vec_data(x)

  expect_equal(anyDuplicated(x), anyDuplicated(x_base))
})

test_that("matrix/array results are the same as base R", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(anyDuplicated(x), anyDuplicated(x_base))

  expect_equal(
    anyDuplicated(x, MARGIN = 2),
    anyDuplicated(x_base, MARGIN = 2)
  )

  expect_equal(
    anyDuplicated(x, MARGIN = c(1, 2)),
    anyDuplicated(x_base, MARGIN = c(1, 2))
  )

  expect_equal(
    anyDuplicated(x, MARGIN = 0),
    anyDuplicated(x_base, MARGIN = 0)
  )
})

test_that("incomparables is an error from base R", {
  expect_error(anyDuplicated(rray(1), incomparables = TRUE))
})

test_that("dim names are kept with base R rules", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3), dim_names = list(letters[1:2], letters[3:5]))
  x_base <- vec_data(x)

  expect_equal(
    rray_dim_names(anyDuplicated(x)),
    rray_dim_names(anyDuplicated(x_base))
  )

  expect_equal(
    rray_dim_names(anyDuplicated(x, MARGIN = c(1, 2))),
    rray_dim_names(anyDuplicated(x_base, MARGIN = c(1, 2)))
  )
})

test_that("fromLast works", {
  x <- rray(c(1, 2, 1, 2, 3, 5), dim = c(2, 3))
  x_base <- vec_data(x)

  expect_equal(
    anyDuplicated(x, MARGIN = 2, fromLast = TRUE),
    anyDuplicated(x_base, MARGIN = 2, fromLast = TRUE)
  )

  expect_equal(
    anyDuplicated(x, MARGIN = c(1, 2), fromLast = TRUE),
    anyDuplicated(x_base, MARGIN = c(1, 2), fromLast = TRUE)
  )
})
