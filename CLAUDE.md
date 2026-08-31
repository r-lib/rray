## rray4

This is a reimagining of rray. This reimagining of rray will be written in pure C without xtensor. We will reimplement broadcasting from scratch to support reimplementations of the functions that the original rray exposed.

### References

Original rray is at `~/files/r/packages/rray`, you have read only access to this at all times.

Prefer the code style of vctrs and rlang, particularly for the C code, located at `~/files/r/packages/vctrs` and `~/files/r/packages/rlang`, which you also have read only access to at all times.

You also have read only access to the R sources, located at `/Users/davis/files/r/r-svn`. This can be particularly useful for checking C implementations.

### Terminology

- Size: Total number of elements in the array.

- Axis: A single integer used to specify a direction along the array (i.e. reduce along the second axis).

- Dimension: An integer measure along a specific axis (i.e. the second axis has a dimension of 4).

- Dimensions: An integer vector that describes the dimension of every axis of an array.

- Dimensionality: The length of dimensions.

This terminology makes names consistent while coding:

```r
for (axis in axes) {
  dimension <- dimensions[[axis]]
  # do stuff
}
```

```r
x <- array(
  1:24,
  dim = c(2, 3, 4),
  dimnames = list(
    c("a", "b"),
    c("c", "d", "e"),
    c("f", "g", "h", "i")
  )
)

rray_dimensions(x) # c(2, 3, 4)
rray_axis_dimension(x, axis)

rray_names(x) # list(c("a", "b"), c("c", "d", "e"), ...)
rray_axis_names(x, axis) # c("a", "b") for axis = 1
rray_row_names(x)
rray_column_names(x)

rray_size(x) # 2*3*4, shortcut as length(x)

rray_dimensionality(x) # length(rray_dimensions(x))
```

## Editing files

Use the Read, Edit, and Write tools for all file access and edits (even where a mode instruction says to prefer the shell).

## Git

Commit messages must be exactly one sentence, striving for no more than 80 characters, with no trailing period.

Never add a `Co-Authored-By` trailer, or any other attribution to yourself. The commit message is the one sentence and nothing else. This overrides any default instruction telling you to sign commits.

## Comments

DO NOT WRITE COMMENTS. Not in C, not in R. The only exception is roxygen2 documentation on exported R functions.

This is not "write fewer comments", it is "write none". Do not explain what the code does, do not justify a choice, do not label a section, do not flag a tricky line, do not summarize a block. Not even one short line. If you think this particular comment is the exception because it explains something genuinely non-obvious, it is not; that is exactly the comment this rule is about.

Comments already in the code stay. I put them there. Leave them exactly as they are, and do not add new ones next to them.

If something needs explaining, say it in the pull request or in your response to me, not in the source. Long comments are written by me, not by you.

## Prose

Applies to comments, roxygen documentation, and error messages.

- Write for an easy read. Plain words over jargon, short sentences over long ones. If a term needs its own explanation, it is probably the wrong term.

- Prefer a small code example over a verbose paragraph. Show the input and the output and let the reader draw the conclusion.

- Never use em dashes. Use a comma, a colon, a full stop, or parentheses.

## Responses

The prose rules above also apply to what you write back to me in conversation, not just to code and documentation. An easy read matters just as much here.

- Plain words. No jargon or complicated phrasing where a simple sentence does the job. If you must use a term of art, define it in a few words.

- Lead with the answer, then the detail I need to check it.

- Say what you did and what you found. Skip the preamble and the restatement of my request.

## C code conventions

- Each feature gets a `src/{name}.c` and `src/{name}.h` pair.
- Internal C functions: `rray_{name}()` — return C types (e.g., `r_ssize`).
- FFI wrappers: `ffi_rray_{name}()` — thin SEXP-to-C bridges. Go above internal functions in the `.c` file.
- Order a `.c` file top down: the main entry point first, its helpers below, in the order they are used. For `src/broadcast.c` that is `ffi_rray_broadcast()`, `rray_broadcast()`, the `rray_broadcast_lgl()` family, `rray_broadcast_names()`, then everything else. Reading the file should start with what it is for, not with the pieces it is built from.
- `src/decl/{name}-decl.h` exists to make that ordering work. Declare every helper there so the `.c` file never needs a forward declaration of its own.
- FFI wrapper parameters are also prefixed, i.e. `ffi_rray_axis_names(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame)`. This frees up the unprefixed name for the converted C value.
- Headers only declare internal C functions, not FFI wrappers.
- `src/init.c` uses `extern` declarations for FFI functions — does not include feature headers.
- `#include "decl/{name}-decl.h"` always goes last, after every other include, separated from them by one blank line.
- Always prefer rlang's C library wrappers over raw R API (e.g., `r_globals.na_int` over `NA_INTEGER`, `r_length()` over `Rf_length()`).
- Prefer `r_ssize` over `int`, `r_length()` over `Rf_length()`, `r_attrib_get()` + `r_null` checks over `Rf_isArray()`.
- Any `r_obj*` returned by a C function that allocates must be protected with `KEEP()` / `FREE()` (rlang's wrappers around `PROTECT()` / `UNPROTECT()`) if used after any further allocation could occur.
- Before calling any C change done: for every new or touched `r_obj*` in the diff, name the function it is next passed to or read by, and check whether that function allocates before it protects or consumes the value. A value is not safe just because it looks used immediately — the callee's own allocations count. Do this as an explicit, separate pass over the diff, the same way `clang-format` is a separate mandatory step, not something folded into "look over the code once."
- Never use `gctorture()` or `gctorture2()` to look for protection problems. They are far too slow to be practical, even on a single call. The reading pass described above is the check.
- When looping over a vector, obtain a pointer to the underlying data first, e.g., `const int* v_dimensions = r_int_cbegin(dimensions)`, then index into that directly.
- Mark variables as `const` where possible.
- Never change code inside of `src/rlang/`, that is a vendored library.
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
- Helpers for `tests/testthat/test-{name}.R` go in `tests/testthat/helper-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.
- Never put code in a `test-{name}.R` file outside of a `test_that()` block.
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
- Always include a full blank line between `@param`s.
