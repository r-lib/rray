context("test-inner-cast")

xs <- list(logical(), integer(), double(), character())
tos <- xs

# ------------------------------------------------------------------------------
# Base R inner casting

for (x in xs) {
  for (to in tos) {
    test_that(glue::glue("inner casting {typeof(x)} to {typeof(to)}."), {
      expect_equal(vec_cast_inner(x, to), to)
    })
  }
}

# ------------------------------------------------------------------------------
# Bad `to`

bad_to <- new_vctr(1, class = "unknown")

for (x in xs) {
  test_that(glue::glue("inner casting {typeof(x)} errors with unknown `to`."), {
    expect_error(vec_cast_inner(x, bad_to), class = "vctrs_error_incompatible_cast")
  })
}

# ------------------------------------------------------------------------------
# Bad `x`

bad_x <- new_vctr(1, class = "unknown")

for (to in tos) {
  test_that("inner casting fails with unknown `x`", {
    expect_error(vec_cast_inner(bad_x, to), class = "vctrs_error_incompatible_cast")
  })
}

# ------------------------------------------------------------------------------
# NULL

test_that("inner casting allows NULL `x`", {
  expect_equal(vec_cast_inner(NULL, 1), NULL)
})

test_that("inner casting allows NULL `to`", {
  expect_equal(vec_cast_inner(1, NULL), 1)
})

# ------------------------------------------------------------------------------
# rray to base

xs_rray <- list(rray(numeric()), rray(logical()), rray(integer()))
tos_rray <- xs_rray

for (x in xs_rray) {
  for (to in tos) {
    test_that(glue::glue("inner casting {class(x)[1]} to {typeof(to)}."), {
      expect_equal(
        vec_data(vec_cast_inner(x, to)),
        new_array(vec_cast(vec_data(x), to))
      )
    })
  }
}

# ------------------------------------------------------------------------------
# base to rray

for (x in xs) {
  for (to in tos_rray) {
    test_that(glue::glue("inner casting {typeof(x)} to {class(to)[1]}."), {
      expect_equal(new_array(vec_cast_inner(x, to)), vec_data(to))
    })
  }
}

# ------------------------------------------------------------------------------
# rray default

bad_x <- new_vctr(1, class = "unknown")

for (to in tos_rray) {
  test_that("inner casting rray errors with unknown `x`.", {
    expect_error(vec_cast_inner(bad_x, to), class = "vctrs_error_incompatible_cast")
  })
}

# ------------------------------------------------------------------------------
# rray to rray

for (x in xs_rray) {
  for (to in tos_rray) {
    test_that("inner casting rray to rray.", {
      expect_equal(vec_data(vec_cast_inner(x, to)), new_array(vec_data(to)))
    })
  }
}

# ------------------------------------------------------------------------------
# Attribute testing

test_that("`x` retains shape", {
  x <- matrix(1L)
  expect_equal(vec_cast_inner(x, TRUE), matrix(TRUE))
  expect_equal(vec_cast_inner(x, rray(TRUE)), matrix(TRUE))
})

# ------------------------------------------------------------------------------
# Common inner cast

test_that("common inner cast can be found", {
  expect_equal(vec_cast_inner_common(1, 1L), list(1, 1))
  expect_equal(vec_cast_inner_common(1, rray(1L)), list(1, new_array(1)))
})

test_that("common inner cast with 1 input", {
  expect_equal(vec_cast_inner_common(1), list(1))
})

test_that("common inner cast with no input", {
  expect_equal(vec_cast_inner_common(), list())
})

test_that("can specify ptype", {
  expect_equal(vec_cast_inner_common(1, .to = rray(1L)), list(1L))
})
