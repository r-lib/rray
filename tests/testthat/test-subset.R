context("test-rray-subset")

# ------------------------------------------------------------------------------
# subset

test_that("subset doesn't drop dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # first row
  expect_equal(dim(x[1]), c(1, 4, 2))

  # 1st col of every dimension
  expect_equal(dim(x[,1]), c(3, 1, 2))

  # first 3rd dim
  expect_equal(dim(x[,,1]), c(3, 4, 1))

  # multiple dimension subset
  expect_equal(dim(x[1,1]), c(1, 1, 2))
  expect_equal(dim(x[1,1,1]), c(1, 1, 1))
})

test_that("subset ignores trailing dots", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))
  expect_equal(x[1], x[1,])
  expect_equal(x[,1,], x[,1])
  expect_equal(x[,,1,], x[,,1])
})

test_that("0D slicing", {
  y <- as_rray(matrix(1:10, ncol = 2))
  y_dim <- vec_dim(y)

  # no columns
  expect_equal(vec_dim(y[,0]), c(5L, 0L))

  # no rows
  expect_equal(vec_dim(y[0]), c(0L, 2L))

  expect_error(y[,,0], "Cannot subset")
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

test_that("subset works with base R", {
  x <- array(1:8, dim = c(2, 2, 2), dimnames = list(NULL, NULL, NULL))

  expect_equal(
    rray_subset(x, 1),
    x[1, , , drop = FALSE]
  )

  expect_equal(
    rray_subset(x, 1:2, 1:2, 1),
    new_array(x[1:2, 1:2, 1], c(2, 2, 1))
  )
})

test_that("can't index beyond vector in subset", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_subset(x, 3), "length 2")
  expect_error(rray_subset(x, 1:3), "length 2")
})

test_that("can subset with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_subset(x, TRUE), x)
  expect_equal(rray_subset(x, FALSE), x[0])

  expect_equal(rray_subset(x, TRUE, FALSE), x[,0])

  expect_error(rray_subset(x, c(TRUE, TRUE, TRUE)), "must have length 1 or")

  expect_equal(rray_subset(x, c(TRUE, FALSE)), x[1])
})

test_that("subset with NA (lgl)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_subset(x, NA),
      vec_na(x, n = 2)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA, NA)),
      vec_na(x, n = 2)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA, TRUE)),
      vec_c(vec_na(x, n = 1), x[2])
    )
  )
})

test_that("subset with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_subset(x, NA_integer_),
      vec_na(x, 1)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA_integer_, NA_integer_, NA_integer_)),
      vec_na(x, 3)
    )
  )
})

test_that("subset with NA (real)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_subset(x, NA_real_),
      rray_subset(x, NA_integer_)
    )
  )
})

test_that("subset with character", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  dim_names(x) <- nms

  expect_equal(
    rray_subset(x, "r1"),
    x[1]
  )

  expect_equal(
    rray_subset(x, c("r1", "r2")),
    x
  )

  # mixed integer and character
  expect_equal(
    rray_subset(x, 2, "c1"),
    x[2, 1]
  )
})

test_that("subset with character fails gracefully", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x <- set_row_names(x, c("r1", "r2"))
  expect_error(rray_subset(x, "r3"), "non-existing")

  expect_error(rray_subset(1, "x"), "unnamed")
})

test_that("can't subset past the dimensions of x", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(x[,,,1], "dimension 4")
})

test_that("can use a negative subset", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(x[-1], x[2])
  expect_error(x[-3], "length 2")
})

# ------------------------------------------------------------------------------
# subset assign

test_that("can use a subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 1, 1, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(storage.mode(x), "integer")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 1:2, 1) <- NA
  expect_equal(as.vector(x), c(NA, NA, 3, 4, NA, NA, 7, 8))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))

  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 0, 1) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_subset(x, 1, 1) <- c(1, 2), "Non-broadcastable")
})

test_that("can subset assign with shaped input", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_subset(x, , 1) <- matrix(1:2), NA)
  expect_equal(
    as.vector(x),
    c(1:4, 1:2, 7:8)
  )
})

test_that("can subset assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_subset(x, 1) <- matrix(4:1, nrow = 1)
  expect_equal(as.vector(x), c(4, 2, 3, 4, 2, 6, 1, 8))
})

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

test_that("names are generally not kept with yank", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x <- set_row_names(x, c("a", "b"))
  expect_equal(
    dim_names(rray_yank(x, 1)),
    new_empty_dim_names(1)
  )
})

test_that("names are kept with yank and a 1D array", {
  x <- 1:2
  names(x) <- c("a", "b")
  expect_equal(names(rray_yank(x)), c("a", "b"))
  expect_equal(names(rray_yank(x, 1)), "a")
})

test_that("can yank with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(rep(c(TRUE, FALSE), 4), c(2, 2, 2))
  expect_equal(rray_yank(x, idx), rray(c(1L, 3L, 5L, 7L)))
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

test_that("can yank with a negative index", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(rray_yank(x, -1), rray(2:8))
})

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
  expect_equal(x, rray(c(rep(NA_integer_, 7), 8L), c(2, 2, 2)))
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
  expect_error(rray_yank(x, 1) <- c(1, 2), "Non-broadcastable")
})

test_that("can yank assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_yank(x, 1:4) <- 4:1
  expect_equal(as.vector(x)[1:4], 4:1)
})

# ------------------------------------------------------------------------------
# extract

test_that("extract drops dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # first row
  expect_equal(rray_extract(x, 1), rray_reshape(x[1], length(x[1])))

  # 1st col of every dimension
  expect_equal(rray_extract(x, , 1), rray_reshape(x[,1], length(x[,1])))

  # first 3rd dim
  expect_equal(rray_extract(x, , , 1), rray_reshape(x[,,1], length(x[,,1])))
})

test_that("extract allows subscripts to have >1 length", {
  x <- rray(1:8, c(2, 2, 2))
  expect_equal(
    rray_extract(x, 1:2, 1),
    rray_reshape(x[1:2,1], rray_elems(x[1:2,1]))
  )
})

test_that("extract ignores trailing dots", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))
  expect_equal(rray_extract(x, 1), rray_extract(x, 1,))
  expect_equal(rray_extract(x, , 1), rray_extract(x, , 1,))
  expect_equal(rray_extract(x, , , 1), rray_extract(x, , , 1,))
})

test_that("0D extracting", {
  x <- as_rray(matrix(1:10, ncol = 2))
  expect_equal(rray_extract(x, 0), rray(integer()))
  expect_equal(rray_extract(x, 0, 1), rray(integer()))
  expect_equal(rray_extract(x, , 0), rray(integer()))
})

test_that("extract never keeps dimension names", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  dim_names(x) <- nms
  expect_equal(dim_names(rray_extract(x, 1)), new_empty_dim_names(1))
})

# equivalent to 0
test_that("extract works with `NULL` as dimension", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(rray_extract(x, NULL), rray(integer()))
  expect_equal(rray_extract(x, NULL, 1), rray(integer()))
  expect_equal(rray_extract(x, , NULL), rray(integer()))
})

test_that("extract works with base R", {
  x <- array(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_extract(x, 1),
    as.vector(x[1,,])
  )

  expect_equal(
    rray_extract(x, 1:2, 1:2, 1),
    as.vector(x[,,1])
  )
})

test_that("can't index beyond vector in extract", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_extract(x, 3), "length 2")
  expect_error(rray_extract(x, 1:3), "length 2")
})

test_that("can extract with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_extract(x, TRUE), rray_reshape(x, rray_elems(x)))
  expect_equal(rray_extract(x, FALSE), rray_reshape(x[0], 0))

  expect_equal(rray_extract(x, TRUE, FALSE), rray_reshape(x[0], 0))

  expect_error(rray_extract(x, c(TRUE, TRUE, TRUE)), "must have length 1 or")

  expect_equal(rray_extract(x, c(TRUE, FALSE)), rray(c(1L, 3L, 5L, 7L)))
})

test_that("extract with NA (lgl)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_extract(x, NA),
    rray_reshape(vec_na(x, n = 2), rray_elems(x))
  )

  expect_equal(
    rray_extract(x, c(NA, NA)),
    rray_reshape(vec_na(x, n = 2), rray_elems(x))
  )

  expect_equal(
    rray_extract(x, c(NA, TRUE)),
    rray_reshape(vec_c(vec_na(x, n = 1), x[2]), rray_elems(x))
  )
})

test_that("extract with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_extract(x, NA_integer_),
    rray_reshape(vec_na(x, 1), rray_elems(x[1,]))
  )

  expect_equal(
    rray_extract(x, c(NA_integer_, NA_integer_, NA_integer_)),
    rray_reshape(vec_na(x, 3), rray_elems(x[c(1,1,1)]))
  )
})

test_that("extract with NA (real)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_extract(x, NA_real_),
    rray_extract(x, NA_integer_)
  )
})

test_that("extract with character", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  dim_names(x) <- nms

  expect_equal(
    rray_extract(x, "r1"),
    rray_extract(x, 1)
  )

  expect_equal(
    rray_extract(x, c("r1", "r2")),
    rray_extract(x, c(1, 2))
  )

  # mixed integer and character
  expect_equal(
    rray_extract(x, 2, "c1"),
    rray_extract(x, 2, 1)
  )
})

test_that("extract with character fails gracefully", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x <- set_row_names(x, c("r1", "r2"))
  expect_error(rray_extract(x, "r3"), "non-existing")

  expect_error(rray_extract(1, "x"), "unnamed")
})

test_that("can't extract past the dimensions of x", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_extract(x, , , , 1), "dimension 4")
})

test_that("can use a negative extract", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_extract(x, -1), rray_extract(x, 2))
  expect_error(rray_extract(x, -3), "length 2")
})

# ------------------------------------------------------------------------------
# extract assign

test_that("can use a extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 1, 1, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in integer extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 1) <- NA
  expect_equal(as.vector(x), c(NA, 2, NA, 4, NA, 6, NA, 8))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_extract(x, 1) <- c(1, 2), "Non-broadcastable")
})

test_that("can extract assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_extract(x, 1) <- 4:1
  expect_equal(as.vector(rray_subset(x, 1)), 4:1)
})

# ------------------------------------------------------------------------------
# `[[`

test_that("names are never kept with [[", {
  x <- rray(1:2)
  names(x) <- c("a", "b")
  expect_equal(dim_names(x[[1]]), new_empty_dim_names(1))
})

test_that("cannot use >1 indexer", {
  x <- rray(1:8, c(2, 4))
  expect_error(x[[c(1, 2), c(1, 2)]], "but 2 indexers")
})

test_that("can use positional [[", {
  x <- rray(1:8, c(2, 2, 2))
  expect_equal(x[[3]], rray(3L))
  expect_equal(x[[c(3, 4)]], rray(c(3L, 4L)))
})

test_that("trailing dots are not ignored", {
  x <- rray(1:8, c(2, 2, 2))
  expect_error(x[[1,]], "but 2 indexers")

  x <- rray(1:4, c(2, 2))
  expect_error(x[[1,]], "but 2 indexers")
})

# ------------------------------------------------------------------------------
# `[[<-`

test_that("can use a [[ position assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x[[1]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("can assign to non-contiguous positions", {
  x <- rray(1:8, dim = c(2, 2, 2))
  x[[c(1, 3)]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2, NA, 4:8))
})

test_that("can assign with a logical matrix", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- rray(c(FALSE, TRUE, rep(FALSE, 6)), c(2, 2, 2))
  x[[idx]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(1, NA, 3:8))
})

test_that("can assign with a logical vector", {
  x <- rray(1:8, dim = c(2, 2, 2))
  idx <- c(FALSE, TRUE, rep(FALSE, 6))
  x[[idx]] <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(1, NA, 3:8))
})

test_that("cannot use a [[ index assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(x[[1, 1, 1]] <- NA, "but 3 indexers")
  expect_error(x[[1, 1, 1]] <- NA, "value")
})

test_that("trailing dots are not ignored in `[[<-`", {
  x <- rray(1:8, c(2, 2, 2))
  expect_error(x[[1,]] <- 1, "2 indexers")

  x <- rray(1:4, c(2, 2))
  expect_error(x[[1,]] <- 1, "2 indexers")
})

test_that("assigning NULL in `[[<-` is an error", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(x[[1]] <- NULL, "replacement has length zero")
})
