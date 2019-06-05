library(vctrs)

# ------------------------------------------------------------------------------
context("test-vec-order-compat")

# (flattens to 2D using as.data.frame(), then checks order rowwise)

test_that("can vec_order() with rrays", {
  x <- rray(1:8, c(2, 2, 2))
  x_df <- as.data.frame(x)
  expect_equal(vec_order(x), vec_order(x_df))
  expect_equal(vec_order(x, direction = "desc"), vec_order(x_df, direction = "desc"))
})

# ------------------------------------------------------------------------------
context("test-vec-sort-compat")

# (sorts entire rows together, even across dimensions.
# different from rray_sort()!)

test_that("can vec_sort() with rrays", {
  x <- rray(8:1, c(2, 2, 2))
  expect_equal(vec_sort(x), x[c(2, 1)])
  expect_equal(vec_sort(x, direction = "desc"), x)
})

# ------------------------------------------------------------------------------
context("test-vec-count-compat")

# vec_count(sort = "key") uses vec_order(), so we want to be sure it works

test_that("can vec_count() with rrays", {
  x_core <- rray(8:1, c(2, 2, 2))
  x <- rray_rbind(x_core, x_core[2])

  x_loc <- new_data_frame(n = 2L)
  x_loc$key <- x_core
  x_loc$count <- c(1L, 2L)

  x_count <- new_data_frame(n = 2L)
  x_count$key <- x_core[c(2, 1)]
  x_count$count <- c(2L, 1L)

  x_key <- new_data_frame(n = 2L)
  x_key$key <- x_core[c(2, 1)]
  x_key$count <- c(2L, 1L)

  expect_equal(vec_count(x, sort = "location"), x_loc)
  expect_equal(vec_count(x, sort = "count"), x_count)
  expect_equal(vec_count(x, sort = "key"), x_key)
})

