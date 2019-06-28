context("test-container-type")

xs <- list(logical(), integer(), double(), character())

# ------------------------------------------------------------------------------
# Base R atomic container types

for (x in xs) {
  test_that(glue::glue("container type for {typeof(x)}."), {
    expect_equal(vec_ptype_container(x), logical())
  })
}

# ------------------------------------------------------------------------------
# Bad `x`

bad_x <- new_vctr(1, class = "unknown")

test_that("unknown container types are caught", {
  expect_error(vec_ptype_container(bad_x))
})

# ------------------------------------------------------------------------------
# Unspecified

test_that("unspecified container type is logical()", {
  expect_equal(vec_ptype_container(vctrs::unspecified()), logical())
})

# ------------------------------------------------------------------------------
# NULL

test_that("container type allows NULL `x`", {
  expect_equal(vec_ptype_container(NULL), NULL)
})

# ------------------------------------------------------------------------------
# rray container type

test_that("rray container types are rray(logical())", {
  expect_equal(vec_ptype_container(rray(1)), rray(logical()))
  expect_equal(vec_ptype_container(rray(TRUE)), rray(logical()))
  expect_equal(vec_ptype_container(rray(1L)), rray(logical()))
})

# ------------------------------------------------------------------------------
# common container type

test_that("common container type can be found", {
  expect_equal(vec_ptype_container_common(1, 1L), logical())
})

test_that("common container type with 1 input", {
  expect_equal(vec_ptype_container_common(1), logical())
})

test_that("common container type with no input", {
  expect_equal(vec_ptype_container_common(), NULL)
})

test_that("can specify ptype", {
  expect_equal(vec_ptype_container_common(.ptype = 1), logical())
  expect_equal(vec_ptype_container_common(.ptype = rray(1)), rray(logical()))
})

test_that("can find a common container type with characters", {
  expect_equal(vec_ptype_container_common(1, character()), logical())
})
