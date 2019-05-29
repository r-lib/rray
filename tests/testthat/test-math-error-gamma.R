context("test-math-error-gamma")

# ?pnorm
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)

.fs <- c(
  rray_gamma,
  rray_lgamma,
  rray_digamma,
  rray_trigamma,
  rray_erf,
  rray_erfc
)

.fs_base <- c(
  gamma,
  lgamma,
  digamma,
  trigamma,
  erf,
  erfc
)

.f_names <- c(
  "gamma",
  "lgamma",
  "digamma",
  "trigamma",
  "erf",
  "erfc"
)

for (i in seq_along(.fs)) {
  .f <- .fs[[i]]
  .f_base <- .fs_base[[i]]
  .f_name <- .f_names[[i]]

  test_that(glue::glue("basic - {.f_name}"), {
    expect_equal(.f(rray(2)), rray(suppressWarnings(.f_base(2))))
    expect_equal(.f(rray(TRUE)), rray(.f_base(TRUE)))
    expect_equal(.f(rray(matrix(1:5))), rray(suppressWarnings(.f_base(matrix(1:5)))))
  })

  test_that(glue::glue("dim names are kept - {.f_name}"), {
    nms <- list("r1", "c1")
    x <- rray(1, c(1, 1), dim_names = nms)
    expect_equal(rray_dim_names(.f(x)), rray_dim_names(x))
  })

  test_that(glue::glue("corner cases - {.f_name}"), {
    expect_equal(suppressWarnings(.f(new_array(Inf))), new_array(suppressWarnings(.f_base(Inf))))
    expect_equal(suppressWarnings(.f(new_array(-Inf))), new_array(suppressWarnings(.f_base(-Inf))))

    if (.f_name != "gamma") {
      expect_equal(suppressWarnings(.f(new_array(0L))), new_array(suppressWarnings(.f_base(0))))
    }

    expect_equal(.f(new_array(NaN)), new_array(.f_base(NaN)))
  })

  test_that(glue::glue("vctrs dispatch works - {.f_name}"), {
    expect_equal(.f_base(rray(1)), .f(rray(1)))
    expect_equal(.f_base(rray(1L)), .f(rray(1L)))
  })
}

# ------------------------------------------------------------------------------

# gamma(0) = NaN as it is undefined
# but xtensor gives Inf, which matches the C++ IEEE standard
test_that("rray_gamma(0) is Inf", {
  expect_equal(rray_gamma(0L), new_array(Inf))
})

test_that("negative integers are undefined", {
  suppressWarnings({
    expect_equal(rray_gamma(-1), new_array(gamma(-1)))
    expect_equal(rray_gamma(-2), new_array(gamma(-2)))
  })
})

test_that("Inf value starting at 171.7", {
  expect_true(rray_gamma(171.7) == Inf)
  expect_false(rray_gamma(171.6) == Inf)
})
