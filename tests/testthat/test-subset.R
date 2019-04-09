context("test-rray-subset")

# ------------------------------------------------------------------------------
# yank

test_that("can yank", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_yank(x, 1:2), rray(1:2))
  expect_equal(rray_yank(x, 0), rray(integer()))
})

test_that("can't index beyond vector in a yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_yank(x, 9), "length 8")
  expect_error(rray_yank(x, 8:10), "length 8")
})

test_that("names are not kept with yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x <- set_row_names(x, c("a", "b"))
  expect_equal(
    dim_names(rray_yank(x, 1)),
    new_empty_dim_names(1)
  )
})

test_that("can yank with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(rep(c(TRUE, FALSE), 4), c(2, 2, 2))
  expect_equal(rray_yank(x, idx), rray(c(1, 3, 5, 7)))
})

test_that("shaped logicals with non-identical shape fail with yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- matrix(c(TRUE, FALSE), nrow = 1)
  expect_error(rray_yank(x, idx), "identical to `x`")
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

  expect_equal(
    rray_yank(x, NA),
    rray(NA_integer_, 8)
  )

  expect_equal(
    rray_yank(x, rep(NA, 8)),
    rray(NA_integer_, 8)
  )
})

test_that("yank with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(
    rray_yank(x, NA_integer_),
    rray(NA_integer_)
  )

  expect_equal(
    rray_yank(x, c(NA_integer_, NA_integer_)),
    rray(c(NA_integer_, NA_integer_))
  )
})

test_that("yank with NA (real)", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(
    rray_yank(x, NA_real_),
    rray_yank(x, NA_integer_)
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
  expect_error(rray_yank(x, matrix(1)), ">1 dimensions if it is a logical")
})

# TODO - negative yank

# ------------------------------------------------------------------------------
# subset

test_that("subset doesn't drop dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # 1st col of every dimension
  expect_equal(dim(x[,1]), c(3, 1, 2))

  # first row
  expect_equal(dim(x[1,]), c(1, 4, 2))

  # first 3rd dim
  expect_equal(dim(x[,,1]), c(3, 4, 1))

  # multiple dimension subset
  expect_equal(dim(x[1,1]), c(1, 1, 2))
  expect_equal(dim(x[1,1,1]), c(1, 1, 1))
})

test_that("subset works on 4D", {
  x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))

  expect_equal(dim(x[1, 1, 1, 1]), c(1, 1, 1, 1))
  expect_equal(dim(x[, , , 1]), c(3, 4, 2, 1))
})

test_that("0D slicing", {
  x <- new_rray()
  x_dim <- vec_dim(x)

  # x[i] slicing only available for 1D arrays
  expect_equal(vec_dim(x[0]), x_dim)
  expect_equal(vec_dim(x[0,]), x_dim)

  expect_error(x[,0], "Cannot subset")
  expect_error(x[,,0], "Cannot subset")

  y <- as_rray(matrix(1:10, ncol = 2))
  y_dim <- vec_dim(y)

  # no columns
  expect_equal(vec_dim(y[,0]), c(5L, 0L))

  # no rows
  expect_equal(vec_dim(y[0,]), c(0L, 2L))

  expect_error(y[,,0], "Cannot subset")

  z <- as_rray(array(1, c(1,1,1,1)))
  expect_error(z[,,,,0], "Cannot subset")
})

test_that("subset keeps dimension names", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  dim_names(x) <- nms

  nms1 <- nms
  nms1$r <- nms1$r[1]
  expect_equal(dim_names(rray_subset(x, 1,)), nms1)

  nms2 <- nms
  nms2["r"] <- list(NULL)
  expect_equal(dim_names(x[0,]), nms2)
})

# equivalent to 0
test_that("subset works with `NULL` as dimension", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(x[NULL,], x[0,])
  expect_equal(x[,NULL], x[,0])
  expect_equal(x[NULL,NULL], x[0, 0])
})

# TODO - negative subset

# ------------------------------------------------------------------------------
# yank assign

test_that("can use a yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in integer yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 1:7) <- NA
  expect_equal(x, rray(c(rep(NA, 7), 8), c(2, 2, 2)))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_yank(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in yank assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_yank(x, 1) <- c(1, 2), "Non-recyclable")
})

test_that("can yank assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_yank(x, 1:4) <- 4:1
  expect_equal(as.vector(x)[1:4], 4:1)
})

# ------------------------------------------------------------------------------
# subset assign

# TODO - more subset assign tests

# ------------------------------------------------------------------------------
# pluck

# test_that("extract can pull n-dim elements", {
#   x <- as_rray(array(1:24, dim = c(3, 4, 2)))
#
#   expect_equal(x[[1]], rray(1))
#   expect_equal(x[[2]], rray(2)) # col major
#
#   expect_equal(x[[1, 1, 1]], rray(1))
#   expect_equal(x[[2, 1, 1]], rray(2))
#
#   # not enough dims
#   expect_error(x[[1, 1]])
# })
#
# test_that("extract works on 4D", {
#   x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))
#
#   expect_equal(x[[1]], rray(1))
#   expect_equal(x[[2]], rray(2)) # col major
#
#   expect_equal(x[[1, 1, 1, 1]], rray(1))
#   expect_equal(x[[2, 1, 1, 1]], rray(2))
#   expect_equal(x[[1, 1, 1, 2]], rray(25))
#
#   # not enough dims
#   expect_error(x[[1, 1]])
# })
