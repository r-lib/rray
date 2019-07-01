context("test-inner-type")

xs <- list(logical(), integer(), double(), character())

# ------------------------------------------------------------------------------
# Base R atomic inner types

for (x in xs) {
  test_that(glue::glue("inner type for {typeof(x)}."), {
    expect_equal(vec_ptype_inner(x), x)
  })
}

# ------------------------------------------------------------------------------
# Bad `x`

bad_x <- new_vctr(1, class = "unknown")

test_that("unknown inner types are caught", {
  expect_error(vec_ptype_inner(bad_x))
})

# ------------------------------------------------------------------------------
# Unspecified

test_that("unspecified inner type is logical()", {
  expect_equal(vec_ptype_inner(vctrs::unspecified()), logical())
})

# ------------------------------------------------------------------------------
# NULL

test_that("inner type allows NULL `x`", {
  expect_equal(vec_ptype_inner(NULL), NULL)
})

# ------------------------------------------------------------------------------
# rray inner type

test_that("rray inner types are base R constructor objects", {
  expect_equal(vec_ptype_inner(rray(1)), numeric())
  expect_equal(vec_ptype_inner(rray(TRUE)), logical())
  expect_equal(vec_ptype_inner(rray(1L)), integer())
})

# ------------------------------------------------------------------------------
# common inner type

test_that("common inner type can be found", {
  expect_equal(vec_ptype_inner_common(1, 1L), numeric())
  expect_equal(vec_ptype_inner_common(matrix(1), matrix(1L)), numeric())
  expect_equal(vec_ptype_inner_common(rray(TRUE), matrix(1L)), integer())
})

test_that("common inner type with 1 input", {
  expect_equal(vec_ptype_inner_common(1), numeric())
})

test_that("common inner type with no input", {
  expect_equal(vec_ptype_inner_common(), NULL)
})

test_that("can specify ptype", {
  expect_equal(vec_ptype_inner_common(.ptype = 1), numeric())
  expect_equal(vec_ptype_inner_common(.ptype = rray(1)), numeric())
})

test_that("common inner type errors with characters", {
  expect_error(vec_ptype_inner_common(1, character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype_inner_common(TRUE, character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype_inner_common(1L, character()), class = "vctrs_error_incompatible_type")
})
