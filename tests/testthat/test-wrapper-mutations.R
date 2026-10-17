test_that("logical wrappers are never mutated", {
  expect_no_mutation("logical")
})

test_that("integer wrappers are never mutated", {
  expect_no_mutation("integer")
})

test_that("double wrappers are never mutated", {
  expect_no_mutation("double")
})

test_that("complex wrappers are never mutated", {
  expect_no_mutation("complex")
})

test_that("raw wrappers are never mutated", {
  expect_no_mutation("raw")
})

test_that("character wrappers are never mutated", {
  expect_no_mutation("character")
})

test_that("list wrappers are never mutated", {
  expect_no_mutation("list")
})

test_that("wrappers of ALTREP integers are never mutated", {
  x <- mutation_vector("altrep_integer")
  expect_identical(is_altrep(x), TRUE)
  expect_no_mutation("altrep_integer")
})

test_that("wrappers of ALTREP characters are never mutated", {
  x <- mutation_vector("altrep_character")
  expect_identical(is_altrep(x), TRUE)
  expect_no_mutation("altrep_character")
})
