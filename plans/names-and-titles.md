# Names and titles across several inputs

## Status

Draft. Decisions so far are marked as decided, everything else is in
[Open questions](#open-questions).

## Goal

Every function that builds one result from several inputs should:

- Keep titles, by the same rules the single-array functions already follow.

- Let the caller choose what happens when inputs disagree about names, through
  a `names` argument.

The functions in scope are the ones listed in `plans/titles.md`. Once this plan
is done, that file is deleted.

## Where the work lives

Almost everything goes through `src/broadcast-names.c`:

| Caller | Helper |
|---|---|
| `rray_broadcast()` | `rray_broadcast_names()` |
| Arithmetic, comparison, `rray_equal()`, logical, `rray_pmax()`, `rray_pmin()` | `rray_broadcast_names2()` |
| `rray_combine()`, and `rray_stack()` through it | `rray_broadcast_names_common_opts()` |

`rray_broadcast_common()` calls `rray_broadcast()` once per input, so it never
combines names across inputs. It only needs the title rule for broadcast axes.

`rray_stack()` also goes through `rray_expand_dimensionality()`, which drops
titles today.

## Today's names rules

For each axis, independently:

- An input contributes names only if it has names on that axis and its
  dimension there is not broadcast.

- The first input that contributes wins. Later inputs are ignored, even when
  they disagree.

```r
x <- array(1:2, 2, dimnames = list(c("r1", "r2")))
y <- array(1:2, 2, dimnames = list(c("a", "b")))

rray_names(rray_add(x, y))
# [[1]]
# [1] "r1" "r2"
```

This is per axis, unlike base R, which takes the whole dimnames of the first
input that has any. It becomes the `"coalesce"` mode below.

## Title rules

- A broadcast axis keeps its title (decided). A title labels the axis, not its
  positions, the same reasoning that keeps the title on a reduced axis.

  ```r
  m <- array(1:12, c(1, 3, 4), dimnames = list(A = NULL, B = NULL, C = NULL))

  rray_titles(rray_broadcast(m, c(2, 3, 4)))
  # [1] "A" "B" "C"
  ```

  This also removes today's irregularity where `rray_broadcast()` keeps titles
  only on a no-op.

- The input that supplies an axis's names also supplies its title (decided).

- When two inputs disagree on a title, `"error"` errors (decided). The other
  modes let the first win silently.

- The result has a titles vector exactly when some input has one. Axes nobody
  titled get `""`.

- An axis that ends up with no names takes the first non-empty title among
  all inputs, broadcast ones included (decided). Otherwise a broadcast axis
  would lose its title:

  ```r
  m <- array(1:3, c(1, 3), dimnames = list(Row = "only", Col = c("c1", "c2", "c3")))
  y <- array(1:6, c(2, 3))

  rray_titles(rray_add(m, y))
  # [1] "Row" "Col"
  ```

- Under `"error"`, broadcast inputs still count for titles (decided). If `y`
  above had the title `"Sample"` on axis 1, that is an error.

`"first"` and `"drop"` override these rules with their own, see
[The `names` argument](#the-names-argument).

For `rray_combine()`, the combine axis follows the same rules: its title comes
from the first input that has names on that axis. Its names are still pasted
together from every input, as today.

For `rray_stack()`, the new axis gets the title `""`.

## The `names` argument

A string that picks how names are merged when several inputs have them. In
every mode:

- A broadcast axis contributes no names.

- An input without names on an axis has no say on that axis (decided).

- An input with titles but no names does not count as having names.

- The combine axis of `rray_combine()` still pastes names together, except
  under `"drop"`.

The examples below use:

```r
x <- array(1:2, 2, dimnames = list(c("r1", "r2")))
y <- array(1:2, 2, dimnames = list(c("a", "b")))
```

### `"error"`

The default (decided). Every input with names on an axis must agree exactly,
otherwise error. Titles must agree too (decided). There are no holes:
`c("r1", "")` and `c("r1", "r2")` disagree (decided).

```r
rray_add(x, y, names = "error")
# Error in `rray_add()`:
# ! Can't combine names on axis 1.
# ✖ `x` has names "r1" and "r2".
# ✖ `y` has names "a" and "b".

rray_names(rray_add(x, x, names = "error"))[[1]]
# [1] "r1" "r2"
```

### `"coalesce"`

Today's behavior (decided). For each axis on its own, the first input with
names on that axis wins, and supplies the title too.

```r
rray_names(rray_add(x, y, names = "coalesce"))[[1]]
# [1] "r1" "r2"

a <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
b <- array(1:6, c(2, 3), dimnames = list(NULL, c("c1", "c2", "c3")))

rray_names(rray_add(a, b, names = "coalesce"))
# [[1]]
# [1] "r1" "r2"
#
# [[2]]
# [1] "c1" "c2" "c3"
```

### `"first"`

Like base R (decided). All names come from the first input with names on any
axis. Every other input is ignored, even on axes where the winner has no
names or broadcasts (decided).

```r
rray_names(rray_add(a, b, names = "first"))
# [[1]]
# [1] "r1" "r2"
#
# [[2]]
# NULL
```

The winner supplies every title too, including `""`, and no titles at all if
it has none (decided). When no input has names, there is no winner, and every
axis falls back to the first non-empty title among all inputs.

### `"drop"`

The result has no names and no titles (decided), following the rule that a
function that drops all names drops all titles. The names work is skipped
entirely.

```r
rray_names(rray_add(x, y, names = "drop"))
# NULL
```

### Considered and left out

- Per position coalescing, where the first non-empty name at each position
  wins. Names on one axis could then come from several inputs, which makes
  the title rule murky, and partially named axes are rare.

- `"consensus"`: keep names only when every contributor agrees, otherwise drop
  them on that axis. Silent data loss is the worst outcome of the three, it
  hides the same mistake `"error"` catches.

- A global option like `options(rray.names = "error")`. Global state that
  changes results makes code behave differently depending on who runs it.

### Which functions get it

- Arithmetic, comparison, `rray_equal()`, `rray_not_equal()`, logical,
  `rray_pmax()`, `rray_pmin()`: `names`, after `...`, like `na_rm` on
  `rray_pmax()`.

- `rray_combine()`, `rray_stack()`: `.names`, matching `.axis`, since their
  `...` takes the data.

- Not `rray_broadcast()` or `rray_broadcast_common()`, which never merge names
  across inputs.

- Not `rray_if_else()`, which drops all names on purpose.

## C design

One core helper replaces `rray_broadcast_names()`, `rray_broadcast_names2()`,
and `rray_broadcast_names_common_opts()`:

```c
enum rray_names_mode {
  RRAY_NAMES_ERROR,
  RRAY_NAMES_COALESCE,
  RRAY_NAMES_FIRST,
  RRAY_NAMES_DROP
};

r_obj* rray_broadcast_names_common(
  r_obj* xs,
  r_obj* dimensions,
  enum rray_names_mode mode,
  const bool* v_ignored,
  struct rray_arg* arg,
  struct r_lazy error_call
);
```

It builds the full dimnames list, names and titles together, in one pass over
the inputs. `arg` and `error_call` exist for the `"error"` message.
`rray_broadcast_names()` and `rray_broadcast_names2()` stay as thin wrappers
so call sites don't change shape, but `rray_broadcast_names2()` must not
allocate a list just to call the core.

Titles force the output list to be allocated up front, the same trap as in the
single-array work: a titles-only input has no names to trigger the lazy
allocation.

`"coalesce"` can stop early once every axis has names and a title.

The `names` string is converted to the enum once, in the FFI wrapper, with an
`arg_as_names_mode()` helper in `src/arg.c`.

## Steps

Each step is one pull request.

1. `rray_expand_dimensionality()` keeps titles, with `""` on new axes.

2. The core helper, titles only. `"coalesce"` is the only mode. Covers
   `rray_broadcast()`, `rray_broadcast_common()`, every binary function,
   `rray_combine()`, and `rray_stack()`. Rewrites the `"dimension titles are
   lost"` test in `test-broadcast.R`.

3. `"error"` mode in C, tested through the internal `rray_broadcast_names*()`
   wrappers.

4. `"first"` and `"drop"` modes in C.

5. The `names` and `.names` arguments on every function in scope, with
   documentation. This is where the default becomes `"error"`, so code that
   relied on `"coalesce"` silently picking names starts to error.

## Open questions

1. Deferred: `rray_add()` will grow `...` for n-ary inputs
   (`plans/n-ary.md`). Is `names` after `...` still the right spelling there,
   or should every function in scope use `.names`?
