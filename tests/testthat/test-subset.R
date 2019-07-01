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
  y_dim <- rray_dim(y)

  # no columns
  expect_equal(rray_dim(y[,0]), c(5L, 0L))

  # no rows
  expect_equal(rray_dim(y[0]), c(0L, 2L))

  expect_error(y[,,0], "Cannot subset")
})

test_that("subset keeps dimension names", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  rray_dim_names(x) <- nms

  nms1 <- nms
  nms1$r <- nms1$r[1]
  expect_equal(rray_dim_names(rray_subset(x, 1,)), nms1)

  nms2 <- nms
  nms2["r"] <- list(NULL)
  expect_equal(rray_dim_names(x[0,]), nms2)
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
      vec_init(x, n = 2)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA, NA)),
      vec_init(x, n = 2)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA, TRUE)),
      vec_c(vec_init(x, n = 1), x[2])
    )
  )
})

test_that("subset with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_subset(x, NA_integer_),
      vec_init(x, 1)
    )
  )

  expect_error(
    expect_equal(
      rray_subset(x, c(NA_integer_, NA_integer_, NA_integer_)),
      vec_init(x, 3)
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
  rray_dim_names(x) <- nms

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
  x <- rray_set_row_names(x, c("r1", "r2"))
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

test_that("subset ignored `drop`", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_warning(
    expect_equal(x[1, drop = TRUE], x[1]),
    "`drop` ignored."
  )
})

# ------------------------------------------------------------------------------

test_that("head() works", {
  x <- rray(1:12, dim = c(3, 2, 2))

  expect_equal(head(x), x)

  expect_equal(head(x, 1), x[1])

  expect_equal(head(x, -1), x[1:2])

  expect_error(head(x, c(1, 2)), "1, not 2")
})

test_that("tail() works", {
  x <- rray(1:12, dim = c(3, 2, 2))

  expect_equal(tail(x), x)

  expect_equal(tail(x, 1), x[-(1:2)])

  expect_equal(tail(x, -1), x[2:3])

  expect_error(tail(x, c(1, 2)), "1, not 2")
})
