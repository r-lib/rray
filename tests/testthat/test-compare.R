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
  expect_error(rray_greater(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_greater(1, NULL), new_array(logical()))
  expect_equal(rray_greater(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_greater(1, logical()), new_array(logical()))
  expect_equal(rray_greater(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_greater(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_greater(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_greater(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_greater(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_greater(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
  )
})

test_that("Correct error is thrown with base R types", {
  expect_error(
    rray_greater(matrix(logical(), 0, 2), matrix(logical(), 0, 3)),
    "\\(0, 2\\) and \\(0, 3\\)"
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
  expect_error(rray_greater_equal(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_greater_equal(1, NULL), new_array(logical()))
  expect_equal(rray_greater_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater_equal(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_greater_equal(1, logical()), new_array(logical()))
  expect_equal(rray_greater_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_greater_equal(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_greater_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_greater_equal(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_greater_equal(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_greater_equal(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_greater_equal(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
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
  expect_error(rray_lesser(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_lesser(1, NULL), new_array(logical()))
  expect_equal(rray_lesser(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_lesser(1, logical()), new_array(logical()))
  expect_equal(rray_lesser(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_lesser(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_lesser(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_lesser(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_lesser(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_lesser(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
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
  expect_error(rray_lesser_equal(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_lesser_equal(1, NULL), new_array(logical()))
  expect_equal(rray_lesser_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser_equal(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_lesser_equal(1, logical()), new_array(logical()))
  expect_equal(rray_lesser_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_lesser_equal(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_lesser_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_lesser_equal(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_lesser_equal(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_lesser_equal(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_lesser_equal(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
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
  expect_error(rray_equal(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_equal(1, NULL), new_array(logical()))
  expect_equal(rray_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_equal(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_equal(1, logical()), new_array(logical()))
  expect_equal(rray_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_equal(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_equal(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_equal(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_equal(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_equal(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
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
  expect_error(rray_not_equal(rray(1), "hi"), class = "vctrs_error_incompatible_type")
})

test_that("comparison with NULL return empty common dimension", {
  expect_equal(rray_not_equal(1, NULL), new_array(logical()))
  expect_equal(rray_not_equal(matrix(1), NULL), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_not_equal(array(1, c(1, 2, 2)), NULL), new_array(logical(), c(0, 2, 2)))
})

test_that("0-length dimension behavior", {
  expect_equal(rray_not_equal(1, logical()), new_array(logical()))
  expect_equal(rray_not_equal(matrix(1), logical()), new_matrix(logical(), c(0, 1)))
  expect_equal(rray_not_equal(array(1, c(1, 2, 2)), logical()), new_array(logical(), c(0, 2, 2)))

  expect_equal(rray_not_equal(rep(1, 5), matrix(logical(), 1, 0)), new_array(logical(), c(5, 0)))
})

test_that("common dim of 0 and non-zero/non-one is an error", {
  expect_error(rray_not_equal(c(1, 1), logical()), "\\(2\\) and \\(0\\)")
})

test_that("dimension names are kept", {
  x <- rray(c(TRUE, FALSE), c(1, 2), dim_names = list(c("r1"), c("c1", "c2")))

  expect_equal(rray_dim_names(rray_not_equal(x, 2)), rray_dim_names(x))
  expect_equal(rray_dim_names(rray_not_equal(matrix(1, 1, 2), x)), rray_dim_names(x))

  expect_equal(
    rray_dim_names(rray_not_equal(matrix(logical(0), 0, 2), x)),
    list(NULL, c("c1", "c2"))
  )
})

# ------------------------------------------------------------------------------
context("test-all-equal")

test_that("equality is checked", {
  expect_true(rray_all_equal(1, 1))
  expect_true(rray_all_equal(1L, 1L))
  expect_true(rray_all_equal(matrix(1), matrix(1)))

  expect_false(rray_all_equal(1, 2))
  expect_false(rray_all_equal(1L, 2L))
  expect_false(rray_all_equal(matrix(1), matrix(2)))
})

test_that("dimension names don't matter", {
  x <- rray(1, dim_names = list("hi"))
  expect_true(rray_all_equal(x, rray(1)))
})

test_that("underlying type matters", {
  expect_false(rray_all_equal(1, 1L))
  expect_false(rray_all_equal(matrix(1), matrix(1L)))
})

test_that("class matters", {
  expect_false(rray_all_equal(rray(1), array(1)))
})

test_that("broadcasting is not performed", {
  expect_false(rray_all_equal(c(1, 1), 1))
})

test_that("works with 0 length input", {
  expect_true(rray_all_equal(double(), double()))
  expect_false(rray_all_equal(double(), logical()))
})

test_that("works with NULL input", {
  expect_false(rray_all_equal(double(), NULL))
  expect_equal(rray_all_equal(NULL, NULL), TRUE)
})

# ------------------------------------------------------------------------------
context("test-any-not-equal")

test_that("equality is checked", {
  expect_true(rray_any_not_equal(1, 2))
  expect_true(rray_any_not_equal(1L, 2L))
  expect_true(rray_any_not_equal(matrix(1), matrix(2)))

  expect_false(rray_any_not_equal(1, 1))
  expect_false(rray_any_not_equal(1L, 1L))
  expect_false(rray_any_not_equal(matrix(1), matrix(1)))
})

test_that("dimension names don't matter", {
  x <- rray(1, dim_names = list("hi"))
  expect_false(rray_any_not_equal(x, rray(1)))
})

test_that("underlying type matters", {
  expect_true(rray_any_not_equal(1, 1L))
})

test_that("class matters", {
  expect_true(rray_any_not_equal(rray(1), array(1)))
})

test_that("broadcasting is not performed", {
  expect_true(rray_any_not_equal(c(1, 1), 1))
})

test_that("works with 0 length input", {
  expect_false(rray_any_not_equal(double(), double()))
  expect_true(rray_any_not_equal(double(), logical()))
})

test_that("works with NULL input", {
  expect_true(rray_any_not_equal(double(), NULL))
  expect_equal(rray_any_not_equal(NULL, NULL), FALSE)
})
