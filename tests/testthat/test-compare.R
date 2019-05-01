context("test-greater")

test_that("greater with rrays", {
  expect_is(rray_greater(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_greater(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) > 1, rray(FALSE))
})

test_that("greater with base R", {
  expect_equal(rray_greater(1, 2), new_array(FALSE))

  expect_equal(
    rray_greater(matrix(1:5), matrix(6:10)),
    new_matrix(FALSE, c(5, 1))
  )
})

test_that("ensure greater requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_greater(x, matrix(1:2)),
    new_array(c(rep(FALSE, 2), rep(TRUE, 6)), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") > rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_greater(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_greater(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_greater(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_greater(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_greater(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_greater(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_greater(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_greater(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_greater(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})

# ------------------------------------------------------------------------------

context("test-greater-equal")

test_that("greater equal with rrays", {
  expect_is(rray_greater_equal(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_greater_equal(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) >= 1, rray(TRUE))
})

test_that("greater equal with base R", {
  expect_equal(rray_greater_equal(1, 1), new_array(TRUE))

  expect_equal(
    rray_greater_equal(matrix(1:5), matrix(6:10)),
    new_matrix(FALSE, c(5, 1))
  )
})

test_that("ensure greater equal requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_greater_equal(x, matrix(1:2)),
    new_array(rep(TRUE, 8), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") >= rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_greater_equal(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_greater_equal(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_greater_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater_equal(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_greater_equal(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_greater_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater_equal(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_greater_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_greater_equal(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_greater_equal(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_greater_equal(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})

# ------------------------------------------------------------------------------

context("test-lesser")

test_that("lesser with rrays", {
  expect_is(rray_lesser(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_lesser(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) < 1, rray(FALSE))
})

test_that("lesser with base R", {
  expect_equal(rray_lesser(1, 1), new_array(FALSE))

  expect_equal(
    rray_lesser(matrix(1:5), matrix(6:10)),
    new_matrix(TRUE, c(5, 1))
  )
})

test_that("ensure lesser requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_lesser(x, matrix(1:2)),
    new_array(rep(FALSE, 8), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") < rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_lesser(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_lesser(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_lesser(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_lesser(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_lesser(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_lesser(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_lesser(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_lesser(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_lesser(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})


# ------------------------------------------------------------------------------

context("test-lesser-equal")

test_that("lesser equal with rrays", {
  expect_is(rray_lesser_equal(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_lesser_equal(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) <= 1, rray(TRUE))
})

test_that("lesser equal with base R", {
  expect_equal(rray_lesser_equal(1, 1), new_array(TRUE))

  expect_equal(
    rray_lesser_equal(matrix(1:5), matrix(6:10)),
    new_matrix(TRUE, c(5, 1))
  )
})

test_that("ensure lesser equal requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_lesser_equal(x, matrix(1:2)),
    new_array(c(rep(TRUE, 2), rep(FALSE, 6)), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") <= rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_lesser_equal(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_lesser_equal(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_lesser_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser_equal(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_lesser_equal(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_lesser_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser_equal(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_lesser_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_lesser_equal(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_lesser_equal(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_lesser_equal(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})

# ------------------------------------------------------------------------------

context("test-equal")

test_that("equal with rrays", {
  expect_is(rray_equal(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_equal(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) == 1, rray(TRUE))
})

test_that("equal with base R", {
  expect_equal(rray_equal(1, 1), new_array(TRUE))

  expect_equal(
    rray_equal(matrix(1:5), matrix(6:10)),
    new_matrix(FALSE, c(5, 1))
  )
})

test_that("ensure equal requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_equal(x, matrix(1:2)),
    new_array(c(rep(TRUE, 2), rep(FALSE, 6)), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") == rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_equal(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_equal(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_equal(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_equal(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_equal(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_equal(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_equal(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_equal(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})

# ------------------------------------------------------------------------------

context("test-not-equal")

test_that("not equal with rrays", {
  expect_is(rray_not_equal(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_not_equal(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) != 1, rray(FALSE))
})

test_that("not equal with base R", {
  expect_equal(rray_not_equal(1, 1), new_array(FALSE))
  expect_equal(rray_not_equal(1, 1L), new_array(FALSE))
  expect_equal(rray_not_equal(1, TRUE), new_array(FALSE))

  expect_equal(
    rray_not_equal(matrix(1:5), matrix(6:10)),
    new_matrix(TRUE, c(5, 1))
  )
})

test_that("ensure not equal requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_not_equal(x, matrix(1:2)),
    new_array(c(rep(FALSE, 2), rep(TRUE, 6)), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") != rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_not_equal(rray(1), "hi"), "No common")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_not_equal(c(1, 1), NULL), new_array(logical()))
  expect_equal(rray_not_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_not_equal(array(1, c(2, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_not_equal(c(1, 1), logical()), new_array(logical()))
  expect_equal(rray_not_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_not_equal(array(1, c(2, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_not_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(2, 2), dim_names = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(dim_names(rray_not_equal(x, 2)), dim_names(x))
  expect_equal(dim_names(rray_not_equal(matrix(1, 2, 2), x)), dim_names(x))

  expect_equal(
    dim_names(rray_not_equal(matrix(logical(0), 2, 0), x)),
    list(c("r1", "r2"), NULL)
  )
})

