# Rename the ALTREP wrapper to a view

After `plans/wrap.md` and `plans/attributes.md`, `r_wrap(x)` returns an
object that:

- shares the data of `x` and never copies it,

- is read only, since R always copies it before changing it,

- starts with no attributes, so the caller sets its own.

That is a read-only view, and "view" says so better than "wrapper" does. Base
R's own wrapper classes behave differently: they copy their data on the first
write and become the owner. Ours never does.

This plan renames everything. It is a pure rename: no behavior changes, apart
from the wording of a few internal error messages.

The work is one pull request. It lands after `plans/attributes.md`, and
assumes the tests that plan adds and renames are in place.

---

# Goal

After this plan, this command:

```sh
git grep -n -i wrap -- ':!src/rlang' ':!plans'
```

prints only these lines, which have nothing to do with the ALTREP class:

- `AGENTS.md`: "FFI wrappers" (3 lines), "rlang's C library wrappers",
  "rlang's wrappers around `PROTECT()`", "Wrap roxygen comments".

- `src/arg.c`, `src/arg.h`, `src/decl/arg-decl.h`, `src/if-else.c`:
  `new_wrapper_arg()` and `wrapper_arg_fill()`. These match vctrs, and stay.

- `src/int-128.h`: "wraps" and "wrapped", about integer overflow.

- `tests/testthat/test-roll.R`: "shifts wrap around the dimension".

`plans/` keeps the old word, since the earlier plans are a record of how we
got here.

---

# Names

## Files

Use `git mv` for each, so history follows the file.

| Old | New |
| --- | --- |
| `src/wrapper.c` | `src/view.c` |
| `src/wrapper.h` | `src/view.h` |
| `src/decl/wrapper-decl.h` | `src/decl/view-decl.h` |
| `tests/testthat/helper-wrapper.R` | `tests/testthat/helper-view.R` |
| `tests/testthat/helper-wrapper-mutations.R` | `tests/testthat/helper-view-mutations.R` |
| `tests/testthat/test-wrapper.R` | `tests/testthat/test-view.R` |
| `tests/testthat/test-wrapper-mutations.R` | `tests/testthat/test-view-mutations.R` |
| `tests/testthat/_snaps/wrapper.md` | deleted, regenerated as `_snaps/view.md` |

Also delete `src/wrapper.o` if it exists, so a stale object file never gets
linked. It is ignored by git.

## C: the entry point and include guard

| Old | New |
| --- | --- |
| `r_wrap()` | `r_view()` |
| `RRAY_WRAPPER_H` | `RRAY_VIEW_H` |
| `#include "wrapper.h"` | `#include "view.h"` |
| `#include "decl/wrapper-decl.h"` | `#include "decl/view-decl.h"` |

## C: internal helpers

| Old | New |
| --- | --- |
| `is_wrapper()` | `is_view()` |
| `wrapper_class()` | `view_class()` |
| `wrapper_data()` | `view_data()` |

## C: class variables and ALTREP class names

For each `{type}` in `logical`, `integer`, `double`, `complex`, `raw`,
`character` and `list`:

| Old | New |
| --- | --- |
| `wrapper_{type}_class` | `view_{type}_class` |
| `"wrapper_{type}"` (the name passed to `R_make_alt*_class()`) | `"view_{type}"` |

The class name shows up in `.Internal(inspect())` and nowhere else. Views
are never serialized as ALTREP (`Serialized_state` returns `NULL`), so no
saved file refers to the old names.

## C: ALTREP methods

| Old | New |
| --- | --- |
| `wrapper_serialized_state()` | `view_serialized_state()` |
| `wrapper_duplicate()` | `view_duplicate()` |
| `wrapper_inspect()` | `view_inspect()` |
| `wrapper_length()` | `view_length()` |

And for each `{type}` that has the method:

| Old | New |
| --- | --- |
| `wrapper_{type}_dataptr()` | `view_{type}_dataptr()` |
| `wrapper_{type}_dataptr_or_null()` | `view_{type}_dataptr_or_null()` |
| `wrapper_{type}_elt()` | `view_{type}_elt()` |
| `wrapper_{type}_get_region()` | `view_{type}_get_region()` |
| `wrapper_character_set_elt()` | `view_character_set_elt()` |
| `wrapper_list_set_elt()` | `view_list_set_elt()` |

## C: initializers

| Old | New |
| --- | --- |
| `init_wrapper_{type}()` | `init_view_{type}()` |
| `r_init_wrapper()` | `r_init_view()` |

## C: test helpers

| Old | New |
| --- | --- |
| `ffi_test_wrap()` | `ffi_test_view()` |
| `ffi_test_wrapper_data()` | `ffi_test_view_data()` |
| `ffi_test_is_wrapper()` | `ffi_test_is_view()` |
| `ffi_test_wrapper_read_access()` | `ffi_test_view_read_access()` |
| `ffi_test_wrapper_pointer_is_stable()` | `ffi_test_view_pointer_is_stable()` |
| `ffi_test_wrapper_set_elt()` | `ffi_test_view_set_elt()` |

`ffi_test_is_altrep()` keeps its name.

## C: messages

| Old | New |
| --- | --- |
| `Can't wrap a %s.` | `Can't create a view of a %s.` |
| `Can't request a writable dataptr to a character wrapper.` | `Can't request a writable dataptr to a character view.` |
| `Can't request a writable dataptr to a list wrapper.` | `Can't request a writable dataptr to a list view.` |
| `Can't set an element of a character wrapper.` | `Can't set an element of a character view.` |
| `Can't set an element of a list wrapper.` | `Can't set an element of a list view.` |
| `` `x` must be a wrapper. `` (2 places) | `` `x` must be a view. `` |
| `<wrapper>\n` in `view_inspect()` | `<view>\n` |

## R test helpers

| Old | New |
| --- | --- |
| `wrap()` | `view()` |
| `wrapper_data()` | `view_data()` |
| `is_wrapper()` | `is_view()` |
| `wrapper_read_access()` | `view_read_access()` |
| `wrapper_pointer_is_stable()` | `view_pointer_is_stable()` |
| `wrapper_set_elt()` | `view_set_elt()` |
| `expect_wraps(w, x)` | `expect_view_of(v, x)` |

`is_altrep()` keeps its name.

Local variables follow: `w` becomes `v`, `w1` and `w2` become `v1` and `v2`,
in all four test and helper files. That includes the argument of every case
function in `helper-view-mutations.R`, for example
`plus = function(v) v + 1`.

## R test descriptions

`test-view.R`:

| Old | New |
| --- | --- |
| wrapping produces an ALTREP wrapper | view() produces an ALTREP view |
| non-wrappers are detected | non-views are detected |
| wrapper preserves length | view preserves length |
| wrapper preserves type | view preserves type |
| wrapper starts with no attributes | view starts with no attributes |
| modifying dim on wrapper does not affect original | modifying dim on a view does not affect the original |
| wrapper provides read access to {type} data (7 tests) | view provides read access to {type} data |
| wrapper starts out sharing data | view starts out sharing data |
| read-only access to a dimensioned wrapper shares data | read-only access to a dimensioned view shares data |
| changing a wrapper gives a plain vector | changing a view gives a plain vector |
| a temporary wrapper is never reused as an output | a temporary view is never reused as an output |
| wrapping an ALTREP vector shares it | a view of an ALTREP vector shares it |
| setting an element of a character or list wrapper errors | setting an element of a character or list view errors |
| wrapping a wrapper does not create nested wrappers | a view of a view does not nest |
| wrapping a wrapper drops its attributes | a view of a view drops its attributes |
| shallow duplicate of a wrapper is a plain vector | shallow duplicate of a view is a plain vector |
| deep copy via c() produces a regular non-wrapper vector | deep copy via c() produces a plain vector |
| serialization round-trips wrapper to plain vector | serialization round-trips a view to a plain vector |
| wrapper works with zero-length vectors | view works with zero-length vectors |
| rray_set_dimensions returns a wrapper | rray_set_dimensions returns a view |
| rray_set_dimensions wrapper shares the underlying data | rray_set_dimensions view shares the underlying data |
| wrapper preserves NAs | view preserves NAs |
| subsetting a wrapper does not produce a wrapper | subsetting a view does not produce a view |
| subsetting a wrapper works | subsetting a view works |

Descriptions without the word keep their text: "a read-only pointer stays
valid after a writable request", "a second change happens in place",
"serialization round-trips all types", "modifying reshaped array does not
affect original".

Section headers in `test-view.R`:

| Old | New |
| --- | --- |
| `# wrap() basics` | `# view() basics` |
| `# Wrapping a wrapper` | `# A view of a view` |

`test-view-mutations.R`:

| Old | New |
| --- | --- |
| {type} wrappers are never mutated (7 tests) | {type} views are never mutated |
| wrappers of ALTREP integers are never mutated | views of ALTREP integers are never mutated |
| wrappers of ALTREP characters are never mutated | views of ALTREP characters are never mutated |

---

# Comments

AGENTS.md says existing comments stay as they are. Davis has asked for these
to change as part of this rename.

## Deleted

These describe the old copy-on-write behavior, so they go rather than get
reworded. Quote each one in the pull request description, so Davis can write
new ones where they are still wanted.

- `src/view.c`, the header block above `r_view()`, from "Wrap an R object in
  a lightweight ALTREP wrapper" to "... also shares it).".

- `src/view.c`, inside `r_view()`, the block from "If `x` is a wrapper,
  unwrap it ..." to "... at that time.".

- `src/view.c`, inside `r_view()`, "Reference count on `data` is bumped when
  we wrap it here. It now looks at least "referenced", but not necessarily
  "shared".".

- `src/view.c`, inside `view_duplicate()`, "Wrapper only guarantees shallow
  ownership." and "No need to return another wrapper if we fully
  duplicate.".

## Reworded

This one is still accurate, so only the word changes:

- `src/utils.c`, above `vec_as_array()`: "we use a lightweight wrapper"
  becomes "we use a lightweight view".

## Kept

- `src/view.c`: "Full serialization, lose ALTREP-ness", "There is no
  `STRING_PTR_RO_OR_NULL`", "There is no `VECTOR_PTR_RO_OR_NULL`".

---

# Checklist

Work through it top to bottom.

## C

- [ ] `git mv` the three C files from the Files table.

- [ ] `src/view.h`: rename the include guard and `r_wrap()`.

- [ ] `src/decl/view-decl.h`: rename `is_wrapper()`, `wrapper_class()` and
      `wrapper_data()`.

- [ ] `src/view.c`: apply every C table above, in file order: includes,
      `r_view()`, class variables, `is_view()`, `view_class()`,
      `view_data()`, the generic methods, then the dataptr, dataptr_or_null,
      elt, get_region and set_elt families, the test helpers, and the
      initializers. Apply the message table as you go.

- [ ] `src/view.c`: delete the comments listed under Deleted.

- [ ] `src/init.c`: rename the six `ffi_test_*` externs and their
      `CallEntries` rows, and `r_init_wrapper()` in both its extern and its
      call in `R_init_rray()`.

- [ ] The 8 files that call `r_wrap()`: switch `#include "wrapper.h"` to
      `#include "view.h"`, keeping the includes sorted, and `r_wrap(` to
      `r_view(`. There are 9 call sites:

  - [ ] `src/dimensionality.c`: `rray_expand_dimensionality()`

  - [ ] `src/dimensions.c`: `rray_set_dimensions()`

  - [ ] `src/insert-axes.c`: `rray_insert_axes()`

  - [ ] `src/remove-axes.c`: `rray_remove_axes()`

  - [ ] `src/names.c`: `rray_set_names()` and `rray_set_axis_names()`

  - [ ] `src/titles.c`: `rray_set_titles()` and `rray_clear_titles()`

  - [ ] `src/utils.c`: `vec_as_array()`, plus the reworded comment

- [ ] Delete `src/wrapper.o` if it exists.

## Tests

- [ ] `git mv` the four test and helper files from the Files table.

- [ ] `helper-view.R`: rename every helper from the R test helpers table,
      and the `.Call()` targets inside them.

- [ ] `helper-view-mutations.R`: rename `w` to `v` in every case function and
      in `expect_no_mutation()`, and `wrapper_data()` to `view_data()`.

- [ ] `test-view-mutations.R`: rename the test descriptions.

- [ ] `test-view.R`: rename the test descriptions, the section headers, the
      helper calls and the local variables.

- [ ] `git rm tests/testthat/_snaps/wrapper.md`, then run the tests once to
      create `_snaps/view.md`. Review it: the calls read `view_set_elt(view(...))`
      and the messages end in "character view." and "list view.".

## Verify

- [ ] Run the command from the Goal section. The output must be exactly the
      list there, and nothing else.

---

# Checks

1. `clang-format -i src/*.c src/*.h`, over all files.

2. `air format .`

3. `Rscript -e "devtools::test()"`, all green, with the same number of tests
   as before the rename.

4. The protection pass from AGENTS.md. A rename adds no allocation, so this
   only confirms no `KEEP()` or `FREE()` moved.

5. No new comments anywhere in `src/` or `tests/`.

6. `Rscript -e "devtools::check()"`.
