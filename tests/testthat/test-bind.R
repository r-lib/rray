context("test-bind")

test_that("can bind simple matrices", {

  a <- matrix(1:2, ncol = 1)
  b <- matrix(3:4, ncol = 1)

  expect_equal(
    rray_bind(a, b, axis = 1),
    new_matrix(1:4, dim = c(4, 1))
  )

  expect_equal(
    rray_bind(a, b, axis = 2),
    new_matrix(1:4, dim = c(2, 2))
  )

})

test_that("can bind up in dimensions", {

  a <- matrix(1:2, ncol = 1)
  b <- matrix(3:4, ncol = 1)

  expect_equal(
    rray_bind(a, b, axis = 3),
    new_array(1:4, c(2, 1, 2))
  )
})

test_that("can broadcast size", {

  a <- matrix(1:2, ncol = 1)
  b <- matrix(3, ncol = 1)

  expect_equal(
    rray_bind(a, b, axis = 2),
    new_matrix(c(1L, 2L, 3L, 3L), c(2, 2))
  )
})

test_that("can broadcast shape", {

  a <- matrix(1:2, ncol = 2)
  b <- matrix(3, ncol = 1)

  expect_equal(
    rray_bind(a, b, axis = 1),
    new_matrix(c(1L, 3L, 2L, 3L), c(2, 2))
  )
})

test_that("can bind >2 at a time", {

  a <- matrix(1:2, ncol = 2)
  b <- matrix(3, ncol = 1)
  c <- matrix(4:5, ncol = 1)

  expect_equal(
    rray_bind(a, b, c, axis = 1),
    new_matrix(c(1L, 3L, 4L, 5L, 2L, 3L, 4L, 5L), c(4, 2))
  )
})

test_that("vectors are cast to matrices before binding", {

  a <- 1:2
  b <- matrix(1L)

  expect_equal(
    rray_bind(a, b),
    new_matrix(c(1L, 2L, 1L), c(3, 1))
  )

})

test_that("vectors can be bound to other vectors", {

  a <- 1:2
  b <- 3:4

  expect_equal(
    rray_bind(a, b),
    new_array(1:4)
  )

  expect_equal(
    rray_bind(a, b, axis = 2),
    new_matrix(1:4, c(2, 2))
  )

})

test_that("a cast to a common type is made", {

  a <- 1:2
  b <- c(3, 4)

  expect_equal(
    storage.mode(rray_bind(a, b)),
    "double"
  )

  a <- rray(1)
  b <- matrix(2)

  expect_equal(
    rray_bind(a, b),
    rray(c(1, 2), c(2, 1))
  )

})

test_that("dim names along the axis are combined", {

  a <- 1:2
  b <- 3:4
  names(a) <- c("a_r1", "a_r2")

  expect_equal(
    dim_names(rray_bind(a, b)),
    list(c(names(a), "", ""))
  )

  names(b) <- c("b_r1", "b_r2")

  expect_equal(
    dim_names(rray_bind(a, b)),
    list(c(names(a), names(b)))
  )

  # dim name names are resolved in order of use
  x <- matrix(1, dimnames = list(x = "x_r1"))
  y <- matrix(2, dimnames = list(y = "y_r1"))

  expect_equal(
    rray_bind(x, y),
    matrix(c(1, 2), dimnames = list(x = c("x_r1", "y_r1")))
  )

  expect_equal(
    rray_bind(y, x),
    matrix(c(2, 1), dimnames = list(y = c("y_r1", "x_r1")))
  )

})

test_that("dim names off the axis follow standard rules", {

  a <- 1:2
  b <- 3:4
  names(a) <- c("a_r1", "a_r2")
  names(b) <- c("b_r1", "b_r2")

  # Only dim names of a are used for rows
  expect_equal(
    dim_names(rray_bind(a, b, axis = 2)),
    list(names(a), NULL)
  )

  # Only dim names of b are used for rows
  expect_equal(
    dim_names(rray_bind(b, a, axis = 2)),
    list(names(b), NULL)
  )

})

test_that("outer dim names are used", {

  a <- 1:2
  b <- 3:4
  names(a) <- c("a_r1", "a_r2")

  expect_equal(
    dim_names(rray_bind(x = a, b, axis = 1)),
    list(c(paste0("x..", names(a)), rlang::names2(b)))
  )

  expect_equal(
    dim_names(rray_bind(x = a, y = b, axis = 1)),
    list(c(paste0("x..", names(a)), "y1", "y2"))
  )

  names(b) <- c("b_r1", "b_r2")

  expect_equal(
    dim_names(rray_bind(x = a, b, axis = 1)),
    list(c(paste0("x..", names(a)), names(b)))
  )

  expect_equal(
    dim_names(rray_bind(x = a, y = b, axis = 1)),
    list(c(paste0("x..", names(a)), paste0("y..", names(b))))
  )

})

test_that("outer dim names are only added to `axis` dimension", {

  x <- matrix(1, dimnames = list("x", "y"))
  y <- matrix(2)

  expect_equal(
    dim_names(rray_bind(x = x, y)),
    list(c("x..x", ""), "y")
  )

})

test_that("outer dim names on new axis dimension are added", {
  x <- matrix(1)
  y <- 2

  expect_equal(
    n_dim_names(rray_bind(x = x, y = y, axis = 3), 3),
    c("x", "y")
  )

})

test_that("can bind with 1 input", {

  expect_equal(
    rray_bind(1),
    new_array(1)
  )

  expect_equal(
    rray_bind(matrix(1)),
    new_matrix(1)
  )

  expect_equal(
    rray_bind(1, axis = 2),
    new_matrix(1)
  )

  expect_equal(
    rray_bind(numeric()),
    new_array(numeric())
  )

  expect_equal(
    rray_bind(numeric(), axis = 2),
    new_matrix(numeric())
  )

})

test_that("can bind with NA values", {

  expect_equal(
    rray_bind(NA, 1),
    new_array(c(NA, 1))
  )

  expect_equal(
    rray_bind(NA),
    new_array(NA)
  )

  expect_equal(
    rray_bind(NA, axis = 2),
    new_array(NA, c(1, 1))
  )

  expect_equal(
    rray_bind(NA, matrix(1)),
    new_array(c(NA, 1), c(2, 1))
  )

  expect_equal(
    row_names(rray_bind(x = NA, matrix(1))),
    c("x", "")
  )

})

test_that("cant use a bad `axis`", {
  expect_error(rray_bind(axis = 0), "Invalid `axis`")
  expect_error(rray_bind(axis = -1), "Invalid `axis`")
})

test_that("can rray_bind() with no input", {
  expect_equal(rray_bind(), NULL)
})

test_that("can rray_rbind() and rray_cbind()", {
  expect_equal(
    rray_rbind(matrix(1), matrix(2)),
    rray_bind(matrix(1), matrix(2), axis = 1)
  )

  expect_equal(
    rray_cbind(matrix(1), matrix(2)),
    rray_bind(matrix(1), matrix(2), axis = 2)
  )
})

# TODO - https://github.com/QuantStack/xtensor-r/issues/103
test_that("broadcasting to same shape internally is fine", {
  a <- matrix(1:4, ncol = 2)
  b <- matrix(5L, ncol = 1)

  expect_equal(
    rray_bind(a, b),
    new_matrix(c(1L, 2L, 5L, 3L, 4L, 5L), c(3, 2))
  )
})
