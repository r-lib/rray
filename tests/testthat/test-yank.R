# ------------------------------------------------------------------------------
# yank

test_that("can yank", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_yank(x, 1:2), new_array(1:2))
  expect_equal(rray_yank(x, 0), new_array(integer()))
})

test_that("yank strips rray class", {
  expect_equal(rray_yank(rray(1), 1), new_array(1))
})

test_that("can't index beyond vector in a yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_yank(x, 9), "length 8")
  expect_error(rray_yank(x, 8:10), "length 8")
})

test_that("names are not kept with yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x <- rray_set_row_names(x, c("a", "b"))
  expect_equal(
    rray_dim_names(rray_yank(x, 1)),
    rray_empty_dim_names(1)
  )
})

test_that("names are NOT kept with yank and a 1D array", {
  x <- 1:2
  names(x) <- c("a", "b")
  expect_equal(names(rray_yank(x)), NULL)
})

test_that("can yank with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(rep(c(TRUE, FALSE), 4), c(2, 2, 2))
  expect_equal(rray_yank(x, idx), new_array(c(1L, 3L, 5L, 7L)))
})

test_that("shaped logicals with non-identical shape fail with yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- matrix(c(TRUE, FALSE), nrow = 1)
  expect_error(rray_yank(x, idx), "size 8")
})

test_that("yank with NULL", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_yank(x, NULL),
    rray_yank(x, 0L)
  )
})

test_that("yank with NA (lgl)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_yank(x, NA),
      rray(NA_integer_, 8)
    )
  )

  expect_error(
    expect_equal(
      rray_yank(x, rep(NA, 8)),
      rray(NA_integer_, 8)
    )
  )
})

test_that("yank with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_yank(x, NA_integer_),
      rray(NA_integer_)
    )
  )

  expect_error(
    expect_equal(
      rray_yank(x, c(NA_integer_, NA_integer_)),
      rray(c(NA_integer_, NA_integer_))
    )
  )
})

test_that("yank with NA (real)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_yank(x, NA_real_),
      rray_yank(x, NA_integer_)
    )
  )
})

test_that("yank with character `i`", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    rray_yank(x, NA_character_),
    "Cannot yank with a character"
  )
})

test_that("cannot yank with non-logical matrix", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_yank(x, matrix(1)), "Can not decrease dimensions", class = "vctrs_error_incompatible_cast")
})

test_that("can yank with a negative index", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(rray_yank(x, -1), new_array(2:8))
})

# ------------------------------------------------------------------------------
# `[[`

test_that("names are never kept with [[", {
  x <- rray(1:2)
  names(x) <- c("a", "b")
  expect_equal(rray_dim_names(x[[1]]), rray_empty_dim_names(1))
})

test_that("cannot use >1 indexer", {
  x <- rray(1:8, c(2, 4))
  expect_error(x[[c(1, 2), c(1, 2)]], "but 2 indexers")
})

test_that("can use positional [[", {
  x <- rray(1:8, c(2, 2, 2))
  expect_equal(x[[3]], new_array(3L))
  expect_equal(x[[c(3, 4)]], new_array(c(3L, 4L)))
})

test_that("trailing dots are not ignored", {
  x <- rray(1:8, c(2, 2, 2))
  expect_error(x[[1,]], "but 2 indexers")

  x <- rray(1:4, c(2, 2))
  expect_error(x[[1,]], "but 2 indexers")
})
