test_that("extract drops dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # first row
  expect_equal(rray_extract(x, 1), new_array(x[1]))

  # 1st col of every dimension
  expect_equal(rray_extract(x, , 1), new_array(x[,1]))

  # first 3rd dim
  expect_equal(rray_extract(x, , , 1), new_array(x[,,1]))
})

test_that("extract allows subscripts to have >1 length", {
  x <- rray(1:8, c(2, 2, 2))
  expect_equal(
    rray_extract(x, 1:2, 1),
    new_array(x[1:2, 1])
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
  expect_equal(rray_extract(x, 0), new_array(integer()))
  expect_equal(rray_extract(x, 0, 1), new_array(integer()))
  expect_equal(rray_extract(x, , 0), new_array(integer()))
})

test_that("extract never keeps dimension names", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  rray_dim_names(x) <- nms
  expect_equal(rray_dim_names(rray_extract(x, 1)), rray_empty_dim_names(1))
})

# equivalent to 0
test_that("extract works with `NULL` as dimension", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_equal(rray_extract(x, NULL), new_array(integer()))
  expect_equal(rray_extract(x, NULL, 1), new_array(integer()))
  expect_equal(rray_extract(x, , NULL), new_array(integer()))
})

test_that("extract works with base R", {
  x <- array(1:8, dim = c(2, 2, 2))

  expect_equal(
    rray_extract(x, 1),
    new_array(x[1,,])
  )

  expect_equal(
    rray_extract(x, 1:2, 1:2, 1),
    new_array(x[,,1])
  )
})

test_that("can't index beyond vector in extract", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_extract(x, 3), "length 2")
  expect_error(rray_extract(x, 1:3), "length 2")
})

test_that("can extract with a logical", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_equal(rray_extract(x, TRUE), new_array(x[TRUE]))
  expect_equal(rray_extract(x, FALSE), new_array(x[0]))

  expect_equal(rray_extract(x, TRUE, FALSE), new_array(x[0]))

  expect_error(rray_extract(x, c(TRUE, TRUE, TRUE)), "must have length 1 or")

  expect_equal(rray_extract(x, c(TRUE, FALSE)), new_array(c(1L, 3L, 5L, 7L)))
})

test_that("extract with NA (lgl)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_extract(x, NA),
      rray_reshape(vec_init(x, n = 2), rray_elems(x))
    )
  )

  expect_error(
    expect_equal(
      rray_extract(x, c(NA, NA)),
      rray_reshape(vec_init(x, n = 2), rray_elems(x))
    )
  )

  expect_error(
    expect_equal(
      rray_extract(x, c(NA, TRUE)),
      rray_reshape(vec_c(vec_init(x, n = 1), x[2]), rray_elems(x))
    )
  )
})

test_that("extract with NA (int)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_extract(x, NA_integer_),
      rray_reshape(vec_init(x, 1), rray_elems(x[1,]))
    )
  )

  expect_error(
    expect_equal(
      rray_extract(x, c(NA_integer_, NA_integer_, NA_integer_)),
      rray_reshape(vec_init(x, 3), rray_elems(x[c(1,1,1)]))
    )
  )
})

test_that("extract with NA (real)", {
  x <- rray(1:8, dim = c(2, 2, 2))

  expect_error(
    expect_equal(
      rray_extract(x, NA_real_),
      rray_extract(x, NA_integer_)
    )
  )
})

test_that("extract with character", {
  x <- rray(1:8, dim = c(2, 2, 2))
  nms <- list(r = c("r1", "r2"), c = c("c1", "c2"), d = c("d1", "d2"))
  rray_dim_names(x) <- nms

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
  x <- rray_set_row_names(x, c("r1", "r2"))
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
