## Session startup

At the START of every session, before doing anything else, create a new conversation file in `conversations/` following the naming convention `{YYYY-MM-DD}-{index}-{topic}.md`. Check existing files to determine the next index.

You MUST keep this file up to date with our FULL conversation. Update it as often as possible. Even record plans that we make.

ALWAYS report an up to date running cost of this conversation at the very top of the file.

## rray4

This is a reimagining of rray. This reimagining of rray will be written in pure C without xtensor. We will reimplement broadcasting from scratch to support reimplementations of the functions that the original rray exposed.

You have write access to this `rray4/` folder at all times.

Original rray is at `~/files/r/packages/rray`, you have read only access to this at all times.

When in doubt, prefer the code style of vctrs and rlang, particularly for the C code, located at `~/files/r/packages/vctrs` and `~/files/r/packages/rlang`, which you also have read only access to at all times.

You also have read only access to the R sources, located at `/Users/davis/files/r/r-svn`. This can be particularly useful for checking C implementations.

### Terminology

- Dimension: A single integer used to specify a direction along the array (i.e. the second dimension)
- Dimension Size: The size/length of a specific dimension (the second dimension has size 4)
- Dimension Sizes: An integer vector of dimension sizes that completely describe the bounds of an array
- Capacity: The total number of elements in an array. `prod(dimension_sizes)`
- Dimensionality: The length of the vector of dimension sizes. The number of dimensions in an array.

This terminology makes names consistent while coding:

```r
for (dimension in dimensions) {
  dimension_size <- dimension_sizes[[dimension]]
  # do stuff
}
```

```r
x <- array(dim = c(2, 3, 4))
rray_dimension_sizes(x) # returns c(2, 3, 4)
rray_dimension_size(x, dimension = 2) # returns 3
rray_capacity(x) # returns 2*3*4
rray_dimensionality(x) # returns 3
```

## C code conventions

- Each feature gets a `src/{name}.c` and `src/{name}.h` pair.
- Internal C functions: `rray_{name}()` — return C types (e.g., `R_xlen_t`).
- FFI wrappers: `ffi_rray_{name}()` — thin SEXP-to-C bridges. Go above internal functions in the `.c` file.
- Headers only declare internal C functions, not FFI wrappers.
- `src/init.c` uses `extern` declarations for FFI functions — does not include feature headers.
- Always prefer rlang's C library wrappers over raw R API (e.g., `r_globals.na_int` over `NA_INTEGER`, `r_length()` over `Rf_length()`).
- Prefer `R_xlen_t` over `int`, `Rf_xlength()` over `Rf_length()`, `Rf_getAttrib()` + `R_NilValue` checks over `Rf_isArray()`.
- Any `r_obj*` returned by a C function that allocates must be protected with `KEEP()` / `FREE()` (rlang's wrappers around `PROTECT()` / `UNPROTECT()`) if used after any further allocation could occur.
- When looping over a vector, obtain a pointer to the underlying data first, e.g., `const int* v_dimension_sizes = r_int_cbegin(dimension_sizes)`, then index into that directly.
- Mark variables as `const` where possible.
- Always run `clang-format -i src/*.c src/*.h` after generating C code. Always run it over all files, not just changed files.

## R package development

### Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# To run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# To run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# To redocument the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

### Coding

* Always run `air format .` after generating code
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)
* Don't use `_$x` or `_$[["x"]]` since this package must work on R 4.1.
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Avoid `expect_true()` and `expect_false()` in favour of a specific expectation which will give a better failure message. A few expectations in newer releases that you might not know about are `expect_all_true()`, `expect_all_equal()`, and `expect_r6_class()`.
- When testing errors and warnings, don't us `expect_error()` or `expect_warning()`. Instead, use `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for warnings because these allow the user to review the full text of the output.
- Avoid the `.package` argument to `local_mocked_bindings()`; this modifies the namespace of another package which is not good practice. Instead create a mockable version of the function in the current package. See `?local_mocked_bindings` for more details.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

### GitHub

- If you use `gh` to retrieve information about an issue, always use `--comments` to read all the comments.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
