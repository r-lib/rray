# Argument tags

## What this is

A plan for one pull request, in two parts.

**Part A**, Parts 1 to 4 below: replace rray's `const char* arg` with a
`struct rray_arg`, ported from vctrs.

**Part B**, Part 5 below: stop `rray_dimensions_common()` accepting `NULL`.

They travel together because Part A rewrites the `rray_dimensions_common()`
loop anyway, and Part B deletes two branches out of the loop Part A is writing.
Landing them apart means writing that loop twice.

It lands as PR 7, before `rray_names_common()`. Everything after it takes `...`
or two array inputs, so every one of those functions wants argument tags on the
day it is written. Converting first means none of them get written twice.

Read Part 1 and Part 2 of `plans/implementation.md` first. This document assumes
the conventions there.

---

# Part 1: Why the current approach runs out

Today an argument name is a `const char*` that is fixed when the function is
compiled:

```c
void check_unclassed(r_obj* x, const char* arg, struct r_lazy error_call);
r_obj* arg_as_array(r_obj* x, const char* arg, struct r_lazy error_call);
```

That works for `x`, `dimensions` and `axes`, which really are fixed. It stops
working the moment a function takes `...`, because the name of an input is only
known at run time, and it is not always a name at all. `rray_broadcast_common()`
had to reach for a stopgap:

```c
const char* arg_from_xs(r_obj* xs_names, r_ssize i, char buffer[RRAY_ARG_SIZE]);
```

It writes `..1` into a caller owned stack buffer, or returns the name when there
is one. It works, and it is 12 lines, but it has three problems.

**It cannot nest.** If `rray_bind()` passes an element of `...` down into
something that itself looks at a list, the inner error can only say `..2`. It
cannot say `..2$foo`, because the outer name is gone by then.

**It cannot be lazy.** A caller who wants to rename the argument, so their own
wrapper's error says `values` rather than `x`, has nowhere to put that.

**It duplicates.** rray already has a second, incompatible flavour of argument
for the ones that get handed to `vctrs::vec_cast()`, which needs an R string:

```c
int arg_as_int(r_obj* x, r_obj* arg, struct r_lazy error_call);
r_obj* arg_as_dimensions(r_obj* dimensions, r_obj* arg, struct r_lazy error_call);
r_obj* arg_as_axes(r_obj* axes, int dimensionality, r_obj* arg, struct r_lazy error_call);
```

So `arg` means `const char*` in some places and `r_obj*` in others, and four
preserved character vectors exist only to feed the second kind.

One type replaces all three.

---

# Part 2: How the vctrs design works

Read `src/arg.c` and `src/arg.h` in vctrs alongside this. It is 340 lines total
and we want most of it.

## The struct

```c
struct vctrs_arg {
  r_obj* shelter;
  struct vctrs_arg* parent;
  r_ssize (*fill)(void* data, char* buf, r_ssize remaining);
  void* data;
};
```

An argument tag is a node in a linked list, walked from the outermost parent
inwards. Each node writes its own piece of the name into a buffer. The pieces
concatenate into something like `x$foo` or `..2[[3]]`.

`fill()` returns the number of bytes written, or a negative number if the buffer
was too small. `data` is whatever that node's constructor stashed. `shelter` is
`r_null` for the nodes that live on the stack, and a real R object for the one
node that has to allocate.

The key move is that nothing is a string until an error is actually raised.
Building a tag costs nothing, so a loop can carry one around and only pay when it
throws.

## Materialising a tag

Two entry points:

```c
r_obj* vctrs_arg(struct vctrs_arg* arg);          // a CHARSXP
const char* vec_arg_format(struct vctrs_arg* arg); // a formatted C string
```

`vctrs_arg()` allocates a 100 byte buffer inside a RAWSXP, walks the list, and
grows the buffer by 1.5x whenever a `fill()` reports it ran out. That loop is
why the tag can be any length.

`vec_arg_format()` runs that, then hands the result to rlang's
`r_format_error_arg()`, which is the same thing cli uses. It returns
`` `x` `` complete with the backticks, in a vmax protected string that R frees
at the end of the enclosing `.Call()`.

## The three constructors

**`new_wrapper_arg(parent, "x")`.** A fixed string. This is what today's
`const char*` becomes. It allocates nothing and lives in a static, which is how
vctrs builds its `vec_args.x`, `vec_args.dot_size` and friends.

**`new_lazy_arg(&lazy)`.** Reads a promise out of a frame when the tag is
materialised. This is how `vec_recycle(x, size, x_arg = "values")` works: the R
function passes `environment()`, C builds
`{.x = syms.x_arg, .env = frame}`, and the promise is only forced if an error
happens.

**`new_subscript_arg(parent, names, n, p_i)`.** The important one. It holds a
**pointer** to an index, not the index:

```c
struct subscript_arg_data {
  struct vctrs_arg self;
  r_obj* names;
  r_ssize n;
  r_ssize* p_i;
};
```

So one arg object serves a whole loop. The caller bumps its own counter and the
tag follows. `vec_recycle_common()` is the model:

```c
r_ssize xs_index = 0;
struct vctrs_arg* p_x_arg = new_subscript_arg(p_xs_arg, xs_names, xs_size, &xs_index);
KEEP(p_x_arg->shelter);

for (r_ssize i = 0; i < xs_size; ++i) {
  ...
  ++xs_index;
}
```

It renders four ways, depending on whether it has a parent and whether the
element is named:

| | named | unnamed |
|---|---|---|
| no parent | `foo` | `..2` |
| has parent | `$foo` | `[[2]]` |

That table is the whole reason nesting reads well. A top level element of `...`
is `..2`, and the same element seen from inside a wrapper is `x[[2]]`.

This is the one constructor that allocates. It puts its data in a RAWSXP and the
names in a two element list, and returns a pointer into that RAWSXP, so the
caller protects `p_arg->shelter`.

---

# Part 3: What rray needs

## Files

New: `src/arg.c`, `src/arg.h`, `src/decl/arg-decl.h`.

`src/arg.c` reads top down as `rray_arg()`, `rray_arg_format()`, then
`new_wrapper_arg()`, `new_lazy_arg()`, `new_subscript_arg()` with their `fill()`
functions beneath each. Every `fill()` and the recursive `fill_arg_buffer()` go
in the decl header.

## The interface

```c
struct rray_arg {
  r_obj* shelter;
  struct rray_arg* parent;
  r_ssize (*fill)(void* data, char* buf, r_ssize remaining);
  void* data;
};

r_obj* rray_arg(struct rray_arg* arg);
const char* rray_arg_format(struct rray_arg* arg);

struct rray_arg new_wrapper_arg(struct rray_arg* parent, const char* arg);
struct rray_arg new_lazy_arg(struct r_lazy* arg);

struct rray_arg* new_subscript_arg(
  struct rray_arg* parent,
  r_obj* names,
  r_ssize n,
  r_ssize* p_i
);
```

Names match vctrs so the two files stay easy to diff against each other. The
struct gets the `rray_` prefix, the constructors do not, matching how
`check_unclassed()` and `arg_as_array()` are already unprefixed helpers.

## Two small helpers to bring along

`r_has_name_at(names, i)` lives in vctrs' `src/utils.c` and is eight lines. It
returns `false` for a non character `names`, and for `NA` and `""` elements. It
belongs in `src/utils.c`.

`r_c_str_format_error_arg(x)` lives in vctrs' `src/rlang-dev.h` and is four
lines. It wraps a bare `const char*` in `r_format_error_arg()` without building
a `struct rray_arg` first, which is what messages about a literal like
`.dimensions` want. It belongs in `src/utils.c` too.

Everything else already exists in the vendored rlang. `r_format_error_arg()`,
`r_lazy_eval()`, `r_is_string()`, `r_alloc_raw()`, `r_raw_begin()`,
`r_stop_internal()` and `R_PRI_SSIZE` are all present, and `r_init_library()` is
already called from `ffi_rray4_init_library()`, so `r_format_error_arg()` is
wired up.

## The globals

`src/utils.c` currently preserves four character vectors that exist only to feed
`vctrs::vec_cast()`:

```c
r_obj* dimensions_chr;
r_obj* dot_dimensions_chr;
r_obj* axes_chr;
r_obj* axis_chr;
```

All four go. They become a `rray_args` struct of static wrapper args, built the
way vctrs builds `vec_args`:

```c
struct rray_args {
  struct rray_arg* empty;
  struct rray_arg* x;
  struct rray_arg* names;
  struct rray_arg* axis;
  struct rray_arg* axes;
  struct rray_arg* dimensions;
  struct rray_arg* dot_dimensions;
};
```

A wrapper arg holds a `const char*` and allocates nothing, so unlike the four
character vectors it replaces, none of this needs `r_preserve()`.

## What we are deliberately not porting

**`src/arg-counter.c`, so `reduce()` and `struct counters`.**

vctrs needs counters because a common size is one number. One `curr_arg` can
describe where that number came from, and `counters_shift()` moves it along as
the reduction picks a new winner.

A common set of dimensions is not one number. Axis 1's winner can be a different
input from axis 2's:

```r
rray_dimensions_common(a = array(1, c(2, 1)), b = array(1, c(1, 3)))
# axis 1 comes from `a`, axis 2 comes from `b`
```

A single `curr_arg` would name the wrong input half the time, so counters do not
fit. Part 4 gives the design that does, and it needs nothing beyond
`new_subscript_arg()`.

Do not add counters later "for symmetry". If a genuinely scalar reduction turns
up, revisit it then and say so in that pull request.

---

# Part 4: The conversion

## Signatures

Every `const char* arg` and every `r_obj* arg` becomes `struct rray_arg* arg`.

`src/utils.h`:

```c
void check_unclassed(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);
r_obj* arg_as_array(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);
int arg_as_int(r_obj* x, struct rray_arg* arg, struct r_lazy error_call);
r_obj* vec_cast(r_obj* x, r_obj* to, struct rray_arg* x_arg, struct rray_arg* to_arg);
```

`arg_from_xs()` and `RRAY_ARG_SIZE` are deleted. `new_subscript_arg()` replaces
them and does the job properly.

`vec_cast()` materialises with `rray_arg()` at the point it builds the call, so
the R string is created only when a cast actually happens.

`src/axes.h`, `src/dimensions.h`, `src/broadcast.h`: `arg_as_axes()`,
`check_axis()`, `arg_as_dimensions()`, `rray_dimensions()`, `rray_broadcast()`
and `check_broadcastable()` all take `struct rray_arg*`.

There are 23 call sites passing a literal `"x"` today. They become
`rray_args.x`.

## Functions that should gain an `arg`

These hardcode `"x"` inside and cannot currently be re-blamed. Give each one an
`arg` parameter and pass `rray_args.x` from its FFI wrapper:

`rray_size()`, `rray_dimensionality()`, `rray_dimension()`,
`rray_set_dimensions()`, `rray_names()`, `rray_axis_names()`,
`rray_set_names()`, `rray_set_axis_names()`, `rray_split()`, `rray_sum()`.

They are all callable from a future function that takes `...`, and every one of
them would report `x` when it means `..3`.

## Messages

`r_format_error_arg()` supplies the backticks, so every message that writes its
own must drop them. There are 17:

```c
// before
r_abort_lazy_call(error_call, "`%s` must be an array, not %s.", arg, ...);

// after
r_abort_lazy_call(error_call, "%s must be an array, not %s.", rray_arg_format(arg), ...);
```

For a literal that is not a `struct rray_arg`, use
`r_c_str_format_error_arg(".dimensions")` instead, the way vctrs does in
`src/size-common.c`.

This should be almost invisible in the snapshots. cli renders `{.arg x}` as
`` `x` `` when styling is off, which is how testthat records them. Check the
snapshot diff anyway and expect it to be empty apart from the messages Part 4
deliberately changes.

## `rray_dimensions_common()`

This is the function that motivates the whole port, so it is worth spelling out.

Today the incompatibility error names one side:

```
Can't find common dimensions at axis 1. `y` has dimension 2, which is
incompatible with dimension 3.
```

The 3 came from some earlier input, and we cannot say which. With subscript args
we can, because the index is a pointer we control:

```c
r_ssize curr_i = 0;
r_ssize next_i = 0;

struct rray_arg* p_curr_arg = new_subscript_arg(p_xs_arg, xs_names, n, &curr_i);
KEEP(p_curr_arg->shelter);
struct rray_arg* p_next_arg = new_subscript_arg(p_xs_arg, xs_names, n, &next_i);
KEEP(p_next_arg->shelter);
```

Alongside `v_out_dimensions`, keep a parallel array recording which input last
set each axis:

```c
r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];
```

An axis starts at a dimension of 1, and a dimension of 1 is compatible with
everything, so an axis can only be in conflict once some input has already set
it. That means `v_out_args[axis]` is always populated by the time it is read.

On conflict, poke both indices and raise:

```c
curr_i = v_out_args[i];
next_i = x_i;

r_abort_lazy_call(
  error_call,
  "Can't find common dimensions at axis %d. "
  "%s has dimension %d and %s has dimension %d.",
  i + 1,
  rray_arg_format(p_curr_arg),
  out_dimension,
  rray_arg_format(p_next_arg),
  x_dimension
);
```

```
Can't find common dimensions at axis 1. `a` has dimension 3 and `b` has
dimension 2.
```

Note both `rray_arg_format()` results are live at once. Both are vmax protected
and neither is freed until the `.Call()` returns, so this is safe.

`rray_broadcast_common()` then drops its `char buffer[RRAY_ARG_SIZE]` and passes
the same subscript arg into `rray_broadcast()`.

## Protection

`new_subscript_arg()` is the only constructor that allocates. `KEEP()` its
`shelter` immediately, the way `vec_recycle_common()` does, and hold it for as
long as the loop runs.

`rray_arg()` allocates the buffer and the result. It is called from inside
`rray_arg_format()`, which protects across the `r_format_error_arg()` call and
frees before returning. Nothing else needs to protect a tag.

The `const char*` that `rray_arg_format()` returns points at vmax memory. Use it
in the `r_abort_lazy_call()` that follows and never store it.

---

# Part 5: Part B, `rray_dimensions_common()` drops `NULL`

## The change

`rray_dimensions_common(NULL, 1:5)` returns `5L` today, because the loop skips
`NULL` inputs. After this, it is an error:

```
`..1` must be an array, not `NULL`.
```

That is the message `rray_dimensions()` already gives, and the message
`rray_broadcast_common()` already gives. Three functions, one answer.

## Why

`rray_dimensions(NULL)` has always been an error. `NULL` is not an array, and
rray has no `NULL` array to point at. `rray_dimensions_common()` accepting what
`rray_dimensions()` rejects is the odd one out, and the only reason it does is
that it was written before there was a rule.

It also removes a wart that PR 6 had to leave in place. `rray_broadcast_common()`
rejects `NULL`, but the rejection arrives from the wrong place, because
`rray_dimensions_common()` runs first and silently drops it:

```r
rray_broadcast_common(NULL)
#> Must supply at least one array to `...`.
```

That reads as though nothing was supplied. Something was, and it was wrong. Once
`NULL` is refused where it is first seen, the message is the accurate one.

**What this costs.** vctrs allows `NULL` in `vec_size_common()` on purpose, so an
optional argument can be threaded through without the caller filtering it. rray
gives that up. A caller holding an array that might be absent has to drop it
before the call:

```r
xs <- Filter(Negate(is.null), xs)
```

That is the trade, and it is worth it while rray has no function whose array
argument is optional. Revisit if one turns up, and if it does, prefer giving that
one function an explicit way to say "absent" over making `NULL` mean two things
everywhere.

## The code

`rray_dimensions_common()` loses its `NULL` branch and its `any` flag:

```c
// Both of these go
if (x == r_null) {
  continue;
}
any = true;
```

The "nothing was supplied" error stays, since `rray_dimensions_common()` with an
empty `...` is still an error. It just tests `n` directly, and it moves to the
top of the function where it reads as the precondition it is:

```c
if (n == 0) {
  r_abort_lazy_call(error_call, "Must supply at least one array to `...`.");
}
```

Check that against `.dimensions`. When `.dimensions` is supplied,
`rray_dimensions_common()` returns before it looks at `...` at all, so
`rray_dimensions_common(.dimensions = c(2L, 3L))` keeps working with an empty
`...`. Keep that early return above the new check.

`rray_broadcast_common()` needs no change. It already loops over every element
and hands each to `rray_broadcast()`, which rejects `NULL` on its own.

## Documentation

`R/dimensions.R`, the `...` parameter:

```r
#' @param ... Arrays. `NULL` inputs are silently dropped.
```

becomes

```r
#' @param ... Arrays.
```

`plans/implementation.md` section 2.2 currently says `rray_dimensions_common()`
drops `NULL` while `rray_broadcast_common()` errors. Replace that with the single
rule: `NULL` is not an array, so every function that takes arrays refuses it.

## Tests

In `tests/testthat/test-dimensions.R`, this test inverts:

```r
test_that("NULL inputs are dropped", {
  expect_identical(rray_dimensions_common(NULL, 1:5, NULL), 5L)
})
```

It becomes an `expect_snapshot(error = TRUE)` covering `NULL` in the first
position and in a later position, so the index in the tag is checked and not just
the refusal.

Keep the existing "errors on zero non-`NULL` inputs" test, but it is now two
different errors rather than one. `rray_dimensions_common()` still reports that
nothing was supplied, while `rray_dimensions_common(NULL, NULL)` now reports that
`..1` is not an array. Snapshot both.

In `tests/testthat/test-broadcast.R`, the existing `rray_broadcast_common(NULL)`
snapshot changes message. That is the wart above being fixed, so accept it and
check the new text says `..1`.

---

# Part 6: Testing Part A

Part B's tests are in Part 5, next to the change they cover. This part is the
argument tag machinery.

There is no exported surface here, so it is tested through the functions that
use it. The R level `rray_dimensions_common()` and `rray_broadcast_common()`
already have most of the shape.

Add, as `expect_snapshot(error = TRUE)`:

- A named input, an unnamed input, and a partially named call, so `..2` and the
  name path are both covered.

- Both sides named in the `rray_dimensions_common()` conflict, with the winner
  coming from a different input per axis. This is the case counters would get
  wrong, so it is the one that has to be pinned.

- An input name long enough to force the buffer to grow past 100 bytes. Nothing
  else exercises that loop.

- A name that is not syntactic, for example `` rray_dimensions_common(`a b` = ...) ``,
  to record what cli does with it.

Plus a plain test that the existing errors are unchanged, which is really the
snapshot diff being empty.

---

# Part 7: Out of scope

**`x_arg` and `call` arguments on the exported R functions.** vctrs has them so
that another package's wrapper can make an error blame its own argument.
`new_lazy_arg()` is ported and ready for exactly that, but nothing in rray calls
for it yet. Adding an argument to every exported function, and documenting it,
is its own pull request. Do it when a real caller wants it, not before.

**`new_subscript_arg_vec()`.** vctrs' convenience wrapper that pulls `names` and
`size` off a vector. rray always has both in hand already.

**`new_counter_arg()` and `reduce()`.** See Part 3.

**Any change to what the errors say**, beyond the two in Part 4 and the ones
Part B forces. The point of this pull request is the machinery. A message that
reads badly today should be fixed in its own commit, so the diff stays
reviewable.

**`NULL` handling anywhere except `rray_dimensions_common()`.** Part B settles
one function, because it is the only one left that disagrees with the rest. Do
not go looking for others to change.
