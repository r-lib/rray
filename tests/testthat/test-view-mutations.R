test_that("logical views are never mutated", {
  expect_no_mutation("logical")
})

test_that("integer views are never mutated", {
  expect_no_mutation("integer")
})

test_that("double views are never mutated", {
  expect_no_mutation("double")
})

test_that("complex views are never mutated", {
  expect_no_mutation("complex")
})

test_that("raw views are never mutated", {
  expect_no_mutation("raw")
})

test_that("character views are never mutated", {
  expect_no_mutation("character")
})

test_that("list views are never mutated", {
  expect_no_mutation("list")
})

test_that("views of ALTREP integers are never mutated", {
  x <- mutation_vector("altrep_integer")
  expect_identical(is_altrep(x), TRUE)
  expect_no_mutation("altrep_integer")
})

test_that("views of ALTREP characters are never mutated", {
  x <- mutation_vector("altrep_character")
  expect_identical(is_altrep(x), TRUE)
  expect_no_mutation("altrep_character")
})
