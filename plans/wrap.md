# Making `r_wrap()` sound

This plan fixes a memory safety bug in our ALTREP wrapper (`src/wrapper.c`),
and makes sure the fix does not cost rray a copy of its inputs on every call.

The work lands as three stacked pull requests. Each one is green and does not
regress performance on its own.

1. `r_unwrap()`: add the complement to `r_wrap()`, and make every data read in
   `src/` go through it.

2. Lock the wrapper: port base R's fix, so a data pointer, once handed out,
   stays valid for as long as the wrapper is reachable.

3. Owned results: stop wrapping arrays that rray itself just allocated, in
   `rray_unstack()` and `rray_stack()`.

Read the whole document before starting. Part 1 explains the bug, Part 2 the
design, and Parts 3 to 5 list every change, pull request by pull request.

---

# Part 1: The bug

## How the wrapper works today

`r_wrap(x)` returns an ALTREP object whose `data1` slot holds the vector being
wrapped. rray uses it to change attributes (dimensions, names, titles) without
copying the data.

When something asks the wrapper for a data pointer, `src/wrapper.c` does this:

- Read-only request (`REAL_RO()`, `r_dbl_cbegin()`): return a pointer straight
  into `data1`.

- Writable request (`REAL()`, `r_dbl_begin()`): if `data1` might be shared,
  copy it, put the copy in `data1`, and return a pointer into the copy.

## What goes wrong

A read-only pointer taken before a writable request points into the old
`data1`, which the wrapper no longer holds.

```c
const double* p = REAL_RO(w);
double* q = REAL(w);
q[0] = 99;
// p[0] == 1, q[0] == 99, w[1] == 99
```

That pointer is stale at best. It can also dangle. Refcounts in R are only an
upper bound: they go down when a slot is overwritten, but not when the object
holding the reference is garbage collected. So the old `data1` can look shared
while nothing reachable holds it:

```r
x <- as.double(1:1e6)
l <- list(x)
w <- wrap(x)
rm(x, l)
```

After the swap, nothing reaches the old vector, and the next GC frees it. With
`MallocScribble=1` on macOS, reading `p[0]` after a GC returns `1.194531e+103`,
which is the `0x5555...` fill pattern of freed memory.

## Base R had the same bug

Base R's own wrapper class (`src/main/altclasses.c`) had exactly this pattern.
Ivan Krylov reported it, and Luke Tierney fixed it in three commits:

- `dbc99e13e8` (r90261, also in R-patched as r90260): the read-only `Dataptr`
  and `Dataptr_or_null` methods now copy shared data too, so a later writable
  request returns the same address.

- `c40bda6593` (r90309, R-devel): once any data pointer has been handed out,
  the wrapper is locked and its data is never replaced again. A shallow
  duplicate of a locked wrapper copies the data instead of sharing it.

- `3580627fa3` (r90315): handles `shallow_duplicate()` returning an object
  with a refcount above zero. This does not apply to us, because
  `r_clone_data()` always allocates a fresh vector.

---

# Part 2: Design

## The lock

The rule, taken from base R:

> No data pointer leaves the wrapper unless the wrapper is the only owner of
> its data. After that, the wrapper's data never changes.

Concretely:

- The first time any data pointer is requested (read-only or writable), the
  wrapper copies its data if the data is shared, then locks itself.

- A locked wrapper never replaces `data1`. Its data has a refcount of exactly
  1, because nothing in rray ever shares locked data (see the next two
  points).

- `r_wrap()` on a locked wrapper copies the data instead of sharing it.

- A shallow duplicate goes through `r_wrap()`, so it copies too.

The lock lives in `data2`, which we do not use today: `r_null` means unlocked,
`r_true` means locked.

`Dataptr_or_null` returns `NULL` for an unlocked wrapper and a pointer for a
locked one. It never copies and never locks. R falls back to `Elt` or
`Get_region` when it gets `NULL`, and we implement both. This differs from
base R, which copies here. `Dataptr_or_null` is meant to be cheap, and R calls
it often (for example in `ITERATE_BY_REGION`), so a copy there would make
plain reads expensive.

## The cost, and `r_unwrap()`

With the lock alone, every official read of a wrapper with shared data costs a
full copy. For rray that would be nearly every call, because `vec_as_array()`
(in `src/utils.c`) wraps every plain vector input, and the user's vector is
still bound in R:

```r
x <- as.double(1:1e6)
rray_sum(x)  # vec_as_array() wraps x, the sum reads it: full copy
```

So inside rray we never read data through the wrapper. We read it from the
vector underneath:

```c
r_obj* x_data = KEEP(r_unwrap(x));
const double* v_x = r_dbl_cbegin(x_data);
```

`r_unwrap(x)` returns `data1` for one of our wrappers and `x` for anything
else. It never copies and never locks.

Why this is safe:

- `x_data` is on our protect stack, so it stays alive even if something swaps
  the wrapper's `data1` while we read. The worst case is a stale view, and
  only if someone writes to `x` through the wrapper while we are reading.
  rray never writes to its inputs.

- `KEEP()` does not change refcounts, so borrowing never makes data look
  shared.

Outside rray, other packages and base R go through the official API. They pay
at most one copy per wrapper, and then get a pointer that stays valid.

## Rules for `x_data`

These hold everywhere in `src/` except `src/wrapper.c` itself, whose
internal helpers (Part 4) hand the wrapped data between each other.

1. Name the unwrapped object `<name>_data`: `x_data`, `y_data`,
   `value_data`, `index_data`, `n_data`, `true_data`, and so on. Never
   `data`.

2. Always `KEEP()` the result of `r_unwrap()`, even when it looks reachable
   through `x`.

3. Create `x_data` on the line right before the `r_*_cbegin()` that reads
   it, and use it for nothing else. `r_length()`, `r_typeof()`, and every
   attribute read stay on `x`. The wrapped vector still carries its own
   original attributes, which can be out of date.

4. `x_data` never leaves the function that creates it. Functions keep taking
   and returning `x`. Never pass `x_data` to another function (the one
   exception is `Rf_match()` in `rray_as_subscript_names()`), never return
   it, never store it in a struct. In particular, never let it reach an R
   object: no `r_list_poke()`, no attribute, no return to R. That would raise
   its refcount for good (refcounts do not go down when a list is garbage
   collected), and a locked wrapper's data must keep a refcount of 1.

5. Never write through `x_data`. Only `r_*_cbegin()` on it, never
   `r_*_begin()`.

## What gets unwrapped

Every `r_<type>_cbegin()` on an object that is often our own wrapper, or that
can be as long as an array input:

- Array payloads: `x`, `y`, `condition`, `true_`, `false_`, `missing`,
  `value`, the elements of `xs`.

- Subscripts and index arrays.

- `n` in `rray_roll_each()`, which goes through `rray_cast()` and
  `rray_broadcast()`.

Out of scope, with the reason:

- Dimensions, axes, and small integer arguments (`times` and `n` in
  `rray_rep()` and `rray_roll()`, `times` in `rray_rep_each()`, `dimensions`
  in `rray_split()`). These go through `arg_as_bare_integer()` and friends,
  which never wrap, so they are only wrappers if a user passes an rray result
  as the argument. Even then the cost is one copy of a short vector, after
  which that wrapper is locked and reads are free.

- Names and titles (`r_list_cbegin(x_names)`, `r_chr_cbegin(axis_names)`).
  Same reasoning: we never wrap them ourselves, and the worst case is one
  copy, after which the wrapper is locked.

- Freshly allocated outputs (`r_*_begin(out)`). These are never wrappers.

## Where to unwrap

At the `r_*_cbegin()` call, and nowhere else. Every site becomes:

```c
r_obj* x_data = KEEP(r_unwrap(x));
const double* v_x = r_dbl_cbegin(x_data);
```

plus one more in the `FREE()` that ends the function or block. In a kernel
macro, these two lines replace the `CONST_DEREF(x)` line, and the macro's
`FREE()` goes up by one.

So drivers, kernel signatures, typedefs, and `src/decl/*-decl.h` headers do
not change. Drivers keep passing `x` and `y`, and only the kernel bodies
change. A driver changes only where it calls `r_*_cbegin()` itself.

Subscripts follow the same rule: `struct rray_subscript` keeps the original
`index` in `.index`, and each reader unwraps it right before its own
`r_*_cbegin()`.

## Existing comments

AGENTS.md says existing comments stay exactly as they are, and no new
comments get written. Some comments in `src/wrapper.c` will no longer match
the code after Part 4 (for example, they mention a `data` variable that
becomes `x_data`, or say "writable handle" where any pointer now counts).
Leave them untouched, and list each one in the pull request description so
Davis can update them. Part 4 lists the ones we know about.

---

# Part 3: Pull request 1, `r_unwrap()`

No behavior change. Every rray data read goes through `r_unwrap()`. Under the
current wrapper this is already sound: the borrowed `x_data` is protected, so
a swap cannot free it.

## `src/wrapper.c` and `src/wrapper.h`

Add to `src/wrapper.h`:

```c
r_obj* r_unwrap(r_obj* x);
```

Add to `src/wrapper.c`, directly below `r_wrap()`:

```c
r_obj* r_unwrap(r_obj* x) {
  return is_wrapper(x) ? wrapper_readonly(x) : x;
}
```

`is_wrapper()` checks `ALTREP(x)` first, so `r_unwrap(r_null)` returns
`r_null`. That matters for `missing` in `rray_if_else()`.

Add a test helper at the bottom of the FFI test helper section, and register
it in `src/init.c`:

```c
r_obj* ffi_test_unwrap_address(r_obj* ffi_x) {
  return r_str_as_character(r_obj_address(r_unwrap(ffi_x)));
}
```

This returns the address as a string, in the same format as
`rlang::obj_address()`, so the unwrapped vector never reaches R (rule 4).

## `src/clone.c`

`r_clone_data()` reads its input. It is called from `wrapper.c` (on data that
is never a wrapper) and from `rray_extract_assign()` and
`rray_slice_assign()` (on `x`, which is often a wrapper).

In each `case`, right before the `r_*_cbegin()`:

```c
case R_TYPE_double: {
  r_obj* x_data = KEEP(r_unwrap(x));
  const double* v_x = r_dbl_cbegin(x_data);
  const r_ssize size = r_length(x);
  r_obj* out = KEEP(r_alloc_double(size));
  double* v_out = r_dbl_begin(out);
  r_memcpy(v_out, v_x, sizeof(double) * size);
  FREE(2);
  return out;
}
```

`r_length(x)` stays on `x`. Every `FREE(1)` becomes `FREE(2)`. Include
`wrapper.h` from `clone.c`.

## Binary operations

File: `src/binary.h`.

`RRAY_BINARY()` is the only place the binary kernels read data. Replace

```c
const X_CTYPE* v_x = X_CONST_DEREF(x);
const Y_CTYPE* v_y = Y_CONST_DEREF(y);
```

with

```c
r_obj* x_data = KEEP(r_unwrap(x));
const X_CTYPE* v_x = X_CONST_DEREF(x_data);

r_obj* y_data = KEEP(r_unwrap(y));
const Y_CTYPE* v_y = Y_CONST_DEREF(y_data);
```

and change the final `FREE(1)` to `FREE(3)`. Include `wrapper.h` from
`binary.h`.

Nothing else changes. Every arithmetic, compare, equality, extremum, and
logical kernel goes through `RRAY_BINARY()`, and the drivers, typedefs, and
kernel signatures keep `x` and `y`.

## Cast

File: `src/cast.c`.

In `RRAY_CAST()` and `RRAY_CAST_LOSSY()`, replace
`const FROM_CTYPE* v_x = FROM_CBEGIN(x);` with

```c
r_obj* x_data = KEEP(r_unwrap(x));
const FROM_CTYPE* v_x = FROM_CBEGIN(x_data);
```

and change `FREE(2)` to `FREE(3)`. `r_length(x)`, `r_dim(x)`, and
`r_dim_names(x)` stay on `x`.

## Broadcast

File: `src/broadcast.c`.

In `RRAY_BROADCAST_ATOMIC()` and `RRAY_BROADCAST_BARRIER()`, replace the
`CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and change
`FREE(1)` to `FREE(2)`.

`rray_broadcast()` and `rray_broadcast_common()` do not change.

## Combine and stack

File: `src/combine.c`.

In `RRAY_COMBINE_FILL_ATOMIC()` and `RRAY_COMBINE_FILL_BARRIER()`, replace
the `CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and add
`FREE(1)` after the `RRAY_COMBINE_FILL_LOOP()`. These macros are `void`
function bodies with no protection of their own today.

`rray_combine()` and `rray_combine_fill()` do not change.

This is what fixes `rray_stack()`. `rray_stack_prepare()` wraps every input
with `rray_insert_axes()`, and those wrappers share the user's data. Today
`rray_combine()` reads them with `r_dbl_cbegin()` and friends. After Part 4,
that read would copy every input in full. With this change it reads the data
underneath, with no copy. `src/stack.c` itself needs no change for this.

## If else

File: `src/if-else.c`.

In `RRAY_IF_ELSE_FILL()`, the three reads at the top become

```c
r_obj* condition_data = KEEP(r_unwrap(condition));
const int* v_condition = r_lgl_cbegin(condition_data);

r_obj* true_data = KEEP(r_unwrap(true_));
CTYPE const* v_true = CBEGIN(true_data);

r_obj* false_data = KEEP(r_unwrap(false_));
CTYPE const* v_false = CBEGIN(false_data);
```

Inside the `missing != r_null` branch, the read becomes

```c
r_obj* missing_data = KEEP(r_unwrap(missing));
CTYPE const* v_missing = CBEGIN(missing_data);
```

with a `FREE(1)` at the end of that branch. The final `FREE(1)` becomes
`FREE(4)`.

`rray_if_else()` and the `rray_if_else_fill()` signature do not change.

## Index

File: `src/index.c`.

- `rray_index()`: the `v_v_index` loop unwraps each index array as it goes.
  The pointers outlive the loop, so every `index_data` stays protected until
  the end of the function:

  ```c
  int n_index_prot = 0;
  const int* v_v_index[RRAY_MAX_DIMENSIONALITY];
  for (r_ssize i = 0; i < indices_size; ++i) {
    r_obj* index_data = KEEP_N(r_unwrap(v_indices[i]), &n_index_prot);
    v_v_index[i] = r_int_cbegin(index_data);
  }
  ```

  Add `FREE(n_index_prot)` next to the final `FREE(5)`. The index arrays come
  from `vec_as_array()`, so they are usually our wrappers. The existing
  comment above the loop stays where it is.

- `rray_any_missing_index()`: in its loop, unwrap `index` right before
  `r_int_cbegin()`, read `index_data`, and `FREE(1)` at the end of each
  iteration, before the `break` check. `r_length(index)` stays on `index`.

- `rray_as_index_array()`: replace `const int* v_x = r_int_cbegin(x);` with
  the unwrap and a read of `x_data`, and change `FREE(1)` to `FREE(2)`.
  `stop_index_array_problem()` takes `v_x`, not `x_data`, so that is fine. It
  still returns `x`.

- In `RRAY_INDEX_ATOMIC()` and `RRAY_INDEX_BARRIER()`, replace the
  `CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and change
  `FREE(1)` to `FREE(2)`.

## Subscripts

Files: `src/subscript.c`, `src/extract-subscript.c`,
`src/slice-subscript.c`.

`struct rray_subscript` keeps the original `index` in `.index`. Every
function that reads its data unwraps right before the read:

- `rray_as_subscript_complement()` and `rray_as_subscript_nonzero()`: in each
  `case`, unwrap `index` right before `r_int_cbegin()` or `r_dbl_cbegin()`.
  In `rray_as_subscript_nonzero()`, change that case's `FREE(1)` to
  `FREE(2)`. In `rray_as_subscript_complement()`, the cases end with
  `break`, so add `FREE(1)` before each `break`.

- `rray_mask_size()`: unwrap `mask` before `r_lgl_cbegin()`, `FREE(1)`
  before `return`. `r_length(mask)` stays on `mask`.

- `rray_subscript_summarise()`: each `case` returns straight from the call.
  Store the result in a local instead, so the unwrap can be freed:

  ```c
  struct rray_subscript_summary out;

  switch (r_typeof(index)) {
  case R_TYPE_integer: {
    r_obj* index_data = KEEP(r_unwrap(index));
    out = rray_subscript_summarise_int(r_int_cbegin(index_data) + start, size);
    FREE(1);
    break;
  }
  case R_TYPE_double: {
    r_obj* index_data = KEEP(r_unwrap(index));
    out = rray_subscript_summarise_dbl(r_dbl_cbegin(index_data) + start, size);
    FREE(1);
    break;
  }
  default:
    r_stop_unreachable();
  }

  return out;
  ```

- `rray_as_subscript_names()`: unwrap right before `r_chr_cbegin()`, and pass
  `index_data` to `Rf_match()` too, since base R may take a data pointer
  there, which would lock the wrapper. Change `FREE(1)` to `FREE(2)`.

- `rray_slice_as_locations()`: in the `locations_dbl` and `mask` cases,
  unwrap `subscript.index` right before the read, and change `FREE(1)` to
  `FREE(2)`. `r_length(subscript.index)` stays as is. The `locations_int`
  case still returns `subscript.index`, which may be a wrapper. Its callers
  unwrap it (see Slice and slice assign).

- The constructors (`rray_as_subscript_locations()`,
  `rray_as_subscript_mask()`, `rray_as_extract_mask()`,
  `rray_as_extract_points()`) and `rray_subscript_as_list()` do not change.
  They pass `index` along, and the functions above do the reads.

## Extract and extract assign

Files: `src/extract.c`, `src/extract-assign.c`.

- In `RRAY_EXTRACT_ATOMIC()` and `RRAY_EXTRACT_BARRIER()`, replace the
  `CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and change
  `FREE(1)` to `FREE(2)`.

- The subscript loop macros are `RRAY_EXTRACT_LOCATIONS_LOOP()`,
  `RRAY_EXTRACT_ATOMIC_MASK_LOOP()`, `RRAY_EXTRACT_BARRIER_MASK_LOOP()`,
  `RRAY_EXTRACT_POINTS_LOOP()`, and the three `RRAY_EXTRACT_ASSIGN_*_LOOP()`
  macros. In each one, the `v_index` line becomes

  ```c
  r_obj* index_data = KEEP(r_unwrap(subscript.index));
  const INDEX_CTYPE* v_index = INDEX_CONST_DEREF(index_data);
  ```

  (`r_lgl_cbegin(index_data)` in the mask loops), and the `do { } while (0)`
  block ends with `FREE(1)`.

- In `RRAY_EXTRACT_ASSIGN_ATOMIC()` and `RRAY_EXTRACT_ASSIGN_BARRIER()`,
  replace the `CONST_DEREF(value)` line with

  ```c
  r_obj* value_data = KEEP(r_unwrap(value));
  const CTYPE* v_value = CONST_DEREF(value_data);
  ```

  and add `FREE(1)` after the `RRAY_EXTRACT_ASSIGN_ITERATE()`.

- `r_length(x)`, `r_length(value)`, and `r_length(subscript.index)` in these
  macros stay as they are.

- `rray_extract()` and `rray_extract_assign()` do not change.
  `r_clone_data(x)` unwraps internally.

## Slice and slice assign

Files: `src/slice.c`, `src/slice-assign.c`.

- `rray_slice()` and `rray_slice_assign()`: `rray_slice_as_locations()` can
  return `subscript.index` itself, which may be a wrapper. Unwrap before the
  read:

  ```c
  r_obj* locations = KEEP_N(rray_slice_as_locations(subscript), &n_prot);
  r_obj* locations_data = KEEP_N(r_unwrap(locations), &n_prot);
  v_v_locations[i] = r_int_cbegin(locations_data);
  ```

  The pointer outlives the loop, so `locations_data` stays protected until
  the final `FREE(n_prot)`, like `locations` already does.

- In `RRAY_SLICE_ATOMIC()` and `RRAY_SLICE_BARRIER()`, replace the
  `CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and change
  `FREE(1)` to `FREE(2)`.

- In `RRAY_SLICE_ASSIGN_ATOMIC()` and `RRAY_SLICE_ASSIGN_BARRIER()`, replace
  the `CONST_DEREF(value)` line with the unwrap and a read of `value_data`,
  and add `FREE(1)` at the end.

These callers of `rray_slice()` and `rray_slice_assign()` need no change:
`rray_reverse()`, `rray_rep()`, `rray_rep_each()`, `rray_roll()`,
`rray_slice_axis()`, `rray_slice_assign_axis()`.

## Split and unstack

File: `src/split.c`.

In `RRAY_SPLIT_FILL_ATOMIC()` and `RRAY_SPLIT_FILL_BARRIER()`, replace the
`CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and add
`FREE(1)` after the `RRAY_SPLIT_FILL_LOOP()`.

`rray_split()` and `rray_split_fill()` do not change. `rray_unstack()` calls
`rray_split()` and is covered by it. Its wrapping of the results is Part 5.

## Permute axes and move axes

File: `src/permute-axes.c`.

In `RRAY_PERMUTE_AXES_ATOMIC()` and `RRAY_PERMUTE_AXES_BARRIER()`, replace
the `CONST_DEREF(x)` line with the unwrap and a read of `x_data`, and change
`FREE(1)` to `FREE(2)`.

`rray_permute_axes()` and `rray_move_axes()` do not change.

## Locate

File: `src/locate.c`.

In `RRAY_LOCATE()`, replace the `CONST_DEREF(x)` line with the unwrap and a
read of `x_data`, and change the final `FREE(2)` to `FREE(3)`. The early
`FREE(1); return out;` for an empty axis comes before the unwrap and does
not change. `r_typeof(x)` stays on `x`.

## Reduce

Files: `src/reduce.h`, `src/reduce-sum.c`, `src/reduce-mean.c`.

- In `RRAY_REDUCE()`, replace the `X_CONST_DEREF(x)` line with the unwrap and
  a read of `x_data`, and change `FREE(1)` to `FREE(2)`. Include `wrapper.h`
  from `reduce.h`.

- The hand-written kernels that return straight from a helper call
  (`rray_sum_lgl()`, `rray_sum_int()`, `rray_sum_int_forced_fallback()`,
  `rray_mean_lgl()`, `rray_mean_int()`, `rray_mean_int_forced_fallback()`)
  unwrap right before their `r_*_cbegin()`, store the helper's result in
  `out`, then `FREE(1)` and return it:

  ```c
  r_obj* x_data = KEEP(r_unwrap(x));
  const int* v_x = r_lgl_cbegin(x_data);

  r_obj* out = rray_sum_lgl_or_int(
    v_x,
    ...
  );

  FREE(1);
  return out;
  ```

  `rray_sum_int()` and `rray_mean_int()` have two return paths (the fallback
  and the normal case), and both do this.

- `rray_sum_dbl()` and `rray_sum_cpl()`: unwrap before the read, and change
  their `FREE(1)` to `FREE(2)`. `rray_mean_dbl()`: move the
  `int n_prot = 0;` above the read, and unwrap with
  `KEEP_N(r_unwrap(x), &n_prot)`.

- `r_length(x)` stays on `x` everywhere. `rray_reduce()` does not change.

## Cumulative

File: `src/cumulative.h`.

In `RRAY_CUMULATIVE()`, replace the `X_CONST_DEREF(x)` line with the unwrap
and a read of `x_data`, and change `FREE(2)` to `FREE(3)`. `r_length(x)`
stays on `x`. Include `wrapper.h` from `cumulative.h`.

`rray_cumulative()` and the kernels do not change.

## Roll each

File: `src/roll-each.c`, and `check_roll_n_not_missing()` in
`src/roll.c`.

- `rray_roll_each()`: replace `const int* v_n = r_int_cbegin(n);` with the
  unwrap and a read of `n_data`, and change `FREE(8)` to `FREE(9)`.

- `rray_roll_each_normalize()`: unwrap `n` right before `r_int_cbegin()`.
  Add `FREE(1)` before the early `return n;`, and change the final `FREE(1)`
  to `FREE(2)`. It still returns `n`, never `n_data`, and reads `r_dim(n)`
  from `n`.

- `check_roll_n_not_missing()` in `src/roll.c`: unwrap `n` right before
  `r_int_cbegin()`, and `FREE(1)` after the loop. This also covers
  `rray_roll()`, where `n` is small, at no extra cost.

- In `RRAY_ROLL_EACH_FILL_ATOMIC()` and `RRAY_ROLL_EACH_FILL_BARRIER()`,
  replace the `CONST_DEREF(x)` line with the unwrap and a read of `x_data`,
  and add `FREE(1)` after the `RRAY_ROLL_EACH_FILL_LOOP()`.
  `rray_roll_each_fill()` does not change.

## Files with nothing to do

These read only dimensions, axes, names, titles, or the `xs` list itself, or
only delegate to the functions above: `src/arg.c`, `src/axes.c`,
`src/broadcast-names.c`, `src/cast-common.c`, `src/dimensionality.c`,
`src/dimensions.c`, `src/insert-axes.c`, `src/move-axes.c`, `src/names.c`,
`src/ptype.c`, `src/ptype-common.c`, `src/reduce-names.c`,
`src/remove-axes.c`, `src/rep.c`, `src/rep-each.c`, `src/reverse.c`,
`src/roll.c` (apart from `check_roll_n_not_missing()`), `src/size.c`,
`src/slice-axis.c`, `src/stack.c`, `src/titles.c`, `src/unstack.c`,
`src/utils.c`.

Before calling the pull request done, check this list: run

```
grep -nE "r_(lgl|int|dbl|cpl|raw|chr|list)_cbegin" src/*.c src/*.h
```

and confirm every hit outside `src/rlang/` is one of: a read of `*_data`, a
read of an object this function allocated, or an out of scope read listed in
Part 2. Macro arguments like `r_dbl_cbegin,` count as reads of whatever the
macro dereferences.

## Tests

In `tests/testthat/helper-wrapper.R`:

```r
unwrap_address <- function(x) {
  .Call(ffi_test_unwrap_address, x)
}
```

In `tests/testthat/test-wrapper.R`, next to the "Wrapping a wrapper" section:

```r
test_that("unwrapping a wrapper gives the wrapped data", {
  x <- c(1, 2, 3)
  w <- wrap(x)
  expect_identical(unwrap_address(w), obj_address(x))
})

test_that("unwrapping a non-wrapper gives the object itself", {
  x <- c(1, 2, 3)
  expect_identical(unwrap_address(x), obj_address(x))
})

test_that("unwrapping does not take ownership", {
  x <- c(1, 2, 3)
  w <- wrap(x)
  unwrap_address(w)
  expect_identical_addresses(wrapper_readonly(w), x)
})
```

Part 4 replaces `wrapper_readonly()` and rewrites the last test along with the
others that use it.

The full test suite is the main check here. Every function's existing tests
exercise the changed kernels.

---

# Part 4: Pull request 2, the lock

All changes are in `src/wrapper.c` and its tests. Keep the file ordered top
down per AGENTS.md: `r_wrap()`, `r_unwrap()`, then the class, data, generic,
dataptr, and element sections as they are today.

## Data section

Replace `wrapper_readonly()` and `wrapper_writable()` with:

```c
static inline r_obj* wrapper_data(r_obj* x) {
  return R_altrep_data1(x);
}

static inline bool wrapper_is_locked(r_obj* x) {
  return R_altrep_data2(x) != r_null;
}

static inline void wrapper_lock(r_obj* x) {
  R_set_altrep_data2(x, r_true);
}

static r_obj* wrapper_owned_data(r_obj* x) {
  r_obj* x_data = wrapper_data(x);

  if (!MAYBE_SHARED(x_data)) {
    return x_data;
  }

  if (wrapper_is_locked(x)) {
    r_stop_internal("Locked wrapper data must not be shared.");
  }

  KEEP(x);
  x_data = r_clone_data(x_data);
  R_set_altrep_data1(x, x_data);
  FREE(1);

  return x_data;
}

static r_obj* wrapper_locked_data(r_obj* x) {
  r_obj* x_data = wrapper_owned_data(x);
  wrapper_lock(x);
  return x_data;
}

static r_obj* wrapper_shareable_data(r_obj* x) {
  r_obj* x_data = wrapper_data(x);

  if (wrapper_is_locked(x)) {
    return r_clone_data(x_data);
  }

  return x_data;
}
```

The existing comment inside `wrapper_writable()` ("If ANYONE besides us
shares the wrapped data, ...") moves with its code into
`wrapper_owned_data()`, above the `MAYBE_SHARED()` branch.

`wrapper_owned_data()` is what `Set_elt` needs: own the data before writing,
but no pointer leaves, so no lock. `wrapper_locked_data()` is for every method
that returns a pointer.

The internal error is the safety net for rule 4 in Part 2. Locked data can
only become shared if something in rray stores an `x_data` in an R object.

Declare all of these in `src/decl/wrapper-decl.h`, and remove the old
declarations.

## `r_wrap()` and `r_unwrap()`

```c
r_obj* r_wrap(r_obj* x) {
  R_altrep_class_t cls = wrapper_class(r_typeof(x));

  r_obj* x_data = KEEP(is_wrapper(x) ? wrapper_shareable_data(x) : x);

  r_obj* out = KEEP(R_new_altrep(cls, x_data, r_null));

  r_attrib_clone_from(out, x);

  FREE(2);
  return out;
}

r_obj* r_unwrap(r_obj* x) {
  return is_wrapper(x) ? wrapper_data(x) : x;
}
```

Keep the existing comments in `r_wrap()` where they are (the snippet leaves
them out). `x_data` needs the `KEEP()` now because `wrapper_shareable_data()`
can allocate, and `R_new_altrep()` allocates after it.

A new wrapper always starts unlocked, because `data2` is `r_null`. When the
input was a locked wrapper, the new wrapper owns a fresh copy (refcount 1),
which is exactly what unlocked data is allowed to be.

## Generic methods

- `wrapper_duplicate()`: deep stays `Rf_duplicate(wrapper_data(x))`. Shallow
  stays `r_wrap(x)`, which now copies when `x` is locked. No other change.

- `wrapper_inspect()`: print `<wrapper locked>` or `<wrapper>`, then the
  subtree as today.

- `wrapper_length()`: `wrapper_data(x)`.

## Dataptr methods

For each atomic type:

```c
static void* wrapper_double_dataptr(r_obj* x, Rboolean writeable) {
  r_obj* x_data = wrapper_locked_data(x);

  if (writeable) {
    return r_dbl_begin(x_data);
  } else {
    return (void*) r_dbl_cbegin(x_data);
  }
}
```

Character and list keep aborting on a writable request, and use
`wrapper_locked_data()` for the read-only one.

## Dataptr_or_null methods

```c
static const void* wrapper_double_dataptr_or_null(r_obj* x) {
  if (!wrapper_is_locked(x)) {
    return NULL;
  }
  return REAL_OR_NULL(wrapper_data(x));
}
```

Character and list do the same, returning `r_chr_cbegin(wrapper_data(x))` and
`r_list_cbegin(wrapper_data(x))` when locked. Their existing comments ("There
is no `STRING_PTR_RO_OR_NULL`") stay.

## Element methods

- `Elt` and `Get_region`: `wrapper_data(x)`. No lock, no copy.

- `wrapper_character_set_elt()` and `wrapper_list_set_elt()`:
  `wrapper_owned_data(x)`.

## Test helpers

In the FFI test helper section:

- Remove `ffi_test_wrapper_writable()`. Nothing uses it.

- Replace `ffi_test_wrapper_readonly()` with
  `ffi_test_wrapper_data_address()`, which returns
  `r_str_as_character(r_obj_address(wrapper_data(ffi_x)))`. Returning the
  data itself to R would break rule 4.

- Add `ffi_test_wrapper_is_locked()`, returning
  `r_lgl(wrapper_is_locked(ffi_x))`.

- Add `ffi_test_wrapper_pointer_is_stable()`. For each atomic type, take the
  read-only pointer first, then the writable one, in two separate statements
  (the evaluation order of the operands of `==` is unspecified in C), and
  return whether they are equal:

  ```c
  case R_TYPE_double: {
    const double* v_read = r_dbl_cbegin(ffi_x);
    double* v_write = r_dbl_begin(ffi_x);
    return r_lgl(v_read == v_write);
  }
  ```

- Add `ffi_test_wrapper_dataptr_or_null_is_null()`, returning
  `r_lgl(DATAPTR_OR_NULL(ffi_x) == NULL)`.

- Extend `ffi_test_wrapper_read_access()` with `character` and `list` cases,
  comparing `v_x[0]` from `r_chr_cbegin()` and `r_list_cbegin()` against
  `r_chr_get()` and `r_list_get()`.

Update `src/init.c` to match: remove the two old entries, add the four new
ones.

In `tests/testthat/helper-wrapper.R`, remove `wrapper_readonly()`,
`wrapper_writable()`, `expect_identical_addresses()`, and
`expect_different_addresses()`, and add:

```r
wrapper_data_address <- function(x) {
  .Call(ffi_test_wrapper_data_address, x)
}
wrapper_is_locked <- function(x) {
  .Call(ffi_test_wrapper_is_locked, x)
}
wrapper_pointer_is_stable <- function(x) {
  .Call(ffi_test_wrapper_pointer_is_stable, x)
}
wrapper_dataptr_or_null_is_null <- function(x) {
  .Call(ffi_test_wrapper_dataptr_or_null_is_null, x)
}

expect_locked <- function(x) {
  expect_identical(wrapper_is_locked(x), TRUE)
}
expect_unlocked <- function(x) {
  expect_identical(wrapper_is_locked(x), FALSE)
}
expect_different_data <- function(x, y) {
  expect_identical(wrapper_data_address(x) == wrapper_data_address(y), FALSE)
}
expect_wraps <- function(w, x) {
  expect_identical(wrapper_data_address(w), obj_address(x))
}
expect_not_wraps <- function(w, x) {
  expect_identical(wrapper_data_address(w) == obj_address(x), FALSE)
}
```

## Existing tests to update

In `tests/testthat/test-wrapper.R`:

- `expect_identical_addresses(wrapper_readonly(w), x)` becomes
  `expect_wraps(w, x)`.

- `expect_different_addresses(wrapper_readonly(w), x)` becomes
  `expect_not_wraps(w, x)`.

- `obj_address(wrapper_readonly(w))` becomes `wrapper_data_address(w)`.

- The Part 3 test "unwrapping does not take ownership" becomes
  `expect_wraps(w, x)`.

- "wrapping a wrapper does not create nested wrappers" checks
  `is_wrapper(wrapper_readonly(w2))`. Replace that line with a check that the
  data addresses of `w1` and `w2` match `obj_address(x)`, which already proves
  the data is the plain `x`.

- "read-only access to a dimensioned wrapper shares data" is no longer true:
  a read-only pointer now takes ownership. Rename it to "read-only access
  takes ownership of shared data", and expect that after
  `wrapper_read_access(w)` the wrapper is locked, its data address differs
  from `x`, and `x` is unchanged.

## New tests

Add a "Locking" section after "Ownership":

```r
test_that("a read-only pointer stays valid after a writable request", {
  x <- c(1, 2, 3)
  w <- wrap(x)
  expect_identical(wrapper_pointer_is_stable(w), TRUE)
  expect_identical(x, c(1, 2, 3))
})

test_that("a new wrapper is unlocked", {
  expect_unlocked(wrap(c(1, 2, 3)))
})

test_that("element access does not lock", {
  w <- wrap(c(1, 2, 3))
  expect_identical(w[2], 2)
  expect_unlocked(w)
})

test_that("dataptr_or_null is NULL until the wrapper is locked", {
  w <- wrap(c(1, 2, 3))
  expect_identical(wrapper_dataptr_or_null_is_null(w), TRUE)
  expect_unlocked(w)

  wrapper_read_access(w)
  expect_identical(wrapper_dataptr_or_null_is_null(w), FALSE)
})

test_that("a read-only pointer of unshared data does not copy", {
  w <- wrap(c(1, 2, 3))
  address <- wrapper_data_address(w)
  wrapper_read_access(w)
  expect_locked(w)
  expect_identical(wrapper_data_address(w), address)
})

test_that("wrapping a locked wrapper copies its data", {
  x <- c(1, 2, 3)
  w1 <- wrap(x)
  wrapper_read_access(w1)
  w2 <- wrap(w1)

  expect_unlocked(w2)
  expect_different_data(w2, w1)
  expect_identical(w2[1:3], c(1, 2, 3))
})

test_that("a shallow duplicate of a locked wrapper copies its data", {
  w <- wrap(c(1, 2, 3))
  wrapper_read_access(w)
  w2 <- w
  dim(w2) <- c(1L, 3L)

  expect_identical(is_wrapper(w2), TRUE)
  expect_different_data(w2, w)
})

test_that("set_elt on a locked wrapper writes in place", {
  w <- wrap(c("a", "b"))
  wrapper_read_access(w)
  address <- wrapper_data_address(w)
  w[1] <- "z"
  expect_identical(wrapper_data_address(w), address)
  expect_identical(w[1], "z")
})
```

These rely on R not duplicating `w` itself during `w[1] <- "z"` when only one
binding refers to it. The existing "second write does not re-clone" tests
already depend on that.

## The sweep test

This is the test that proves Part 3 caught every read. After Part 4, a
wrapper gets locked exactly when someone takes an official data pointer from
it. So: give every exported function wrappers in every data position, and
check they are all still unlocked afterwards.

Add a section "rray never takes a data pointer from a wrapper" at the end of
`tests/testthat/test-wrapper.R`. Build fresh wrappers per test with
`rray_set_dimensions()` on vectors bound in the test, so the data is shared.
Do not compare or print results: that would read them, and some results are
the input wrapper itself (for example `rray_broadcast()` with equal
dimensions).

Cover at least:

- `rray_add()`, `rray_subtract()`, `rray_multiply()`, `rray_divide()`,
  `rray_exponentiate()`, `rray_greater_than()`,
  `rray_greater_than_or_equal()`, `rray_less_than()`,
  `rray_less_than_or_equal()`, `rray_equal()`, `rray_not_equal()`,
  `rray_pmax()`, `rray_pmin()`: double wrappers in both `x` and `y`. Also one
  case per op family with broadcasting (`y` of dimensions `c(1L, 3L)`).

- `rray_and()`, `rray_or()`, `rray_xor()`: logical wrappers.

- `rray_broadcast()` to `c(2L, 3L, 2L)`, so the kernel runs.

- `rray_combine()` and `rray_stack()`: two wrappers in `...`.

- `rray_if_else()`: wrappers for `condition`, `true`, `false`, and
  `missing`.

- `rray_index()`: wrappers for `x` and for both index arrays.

- `rray_extract()`: wrapper `x`, and subscript wrappers of each kind:
  integer locations, double locations, negative locations, locations with a
  zero, a logical mask vector, a logical mask array, an integer points matrix,
  a double points matrix.

- `rray_extract_assign()`: wrappers for `x`, `i`, and `value`.

- `rray_slice()`, `rray_slice_axis()`, `rray_slice_rows()`,
  `rray_slice_columns()`: wrapper `x` and wrapper subscripts (integer,
  double, logical, character against named axes).

- `rray_slice_assign()` and `rray_slice_assign_axis()`: wrappers for `x`,
  the subscripts, and `value`.

- `rray_split()` and `rray_unstack()`.

- `rray_permute_axes()`, `rray_move_axes()`, `rray_reverse()`, `rray_rep()`,
  `rray_rep_each()`, `rray_roll()`.

- `rray_roll_each()`: wrappers for `x` and `n`.

- `rray_locate_max()`, `rray_locate_min()`.

- `rray_sum()`, `rray_mean()`, `rray_prod()`, `rray_max()`, `rray_min()`,
  `rray_all()`, `rray_any()`. Include an integer wrapper for `rray_sum()` and
  `rray_mean()`, since their integer kernels are hand written.

- `rray_cumulative_sum()`, `rray_cumulative_prod()`,
  `rray_cumulative_max()`, `rray_cumulative_min()`, `rray_cumulative_all()`,
  `rray_cumulative_any()`.

- The internal `rray_cast()`, with a lossless and a lossy cast.

Any failure here means a read in Part 3 was missed: fix the read, not the
test.

## Stale comments to report

List these in the pull request description, quoted, so Davis can update
them. Do not edit them.

- The header comment of `r_wrap()`: "Read only accesses to the data are
  "passed through" to the wrapped object". Still true for element access,
  but a read-only pointer now takes ownership.

- The header comment of `r_wrap()`: "Writable access forces a shallow
  duplication of the wrapped object if it EVER looks shared". Now any data
  pointer does, and locked wrappers are copied when rewrapped.

- In `r_wrap()`: "If `x` is a wrapper, unwrap it to gain access to the
  underlying data. This is what we wrap and bump the reference count of." A
  locked wrapper's data is copied, and the copy is what gets wrapped.

- In `r_wrap()`: "We manage writable access to the underlying data by always
  cloning it before giving out a writable handle if it is ever
  `MAYBE_SHARED()`". Same.

- In `r_wrap()`: "Reference count on `data` is bumped when we wrap it here."
  The variable is now `x_data`.

- In `r_wrap()`: "Note that we always use `x` here, not `data`." Same.

- In `wrapper_owned_data()`: "clone it before giving out a writable handle".
  Any pointer now.

- In `wrapper_duplicate()`: "Perform shallow duplication by rewrapping the
  wrapped data in a new wrapper." A locked wrapper's data is now copied
  rather than shared.

---

# Part 5: Pull request 3, owned results

`rray_unstack()` and `rray_stack()` call attribute functions on arrays they
just allocated, and those functions wrap. The results are wrappers whose data
nobody else can see. That is correct, but the wrapper buys nothing and costs
an ALTREP indirection on every later element access. Set the attributes
directly instead.

## Ownership

Add `src/ownership.h`, following vctrs (`src/ownership.h` and
`enum vctrs_ownership` in `src/vctrs-core.h` there):

```c
#ifndef RRAY_OWNERSHIP_H
#define RRAY_OWNERSHIP_H

#include "rlang.h"

#include "wrapper.h"

enum rray_ownership {
  RRAY_OWNERSHIP_foreign,
  RRAY_OWNERSHIP_owned
};

static inline r_obj* rray_wrap_foreign(
  r_obj* x,
  enum rray_ownership ownership
) {
  switch (ownership) {
  case RRAY_OWNERSHIP_foreign:
    return r_wrap(x);
  case RRAY_OWNERSHIP_owned:
    return x;
  }

  r_stop_unreachable();
}

#endif
```

`RRAY_OWNERSHIP_owned` means: rray allocated this array at C level, and no R
object other than a list we also own refers to it. Only then may its
attributes be changed in place.

## `rray_remove_axes()`

- Add an `enum rray_ownership ownership` parameter after `axes`, in
  `src/remove-axes.c` and `src/remove-axes.h`.

- Replace `r_obj* out = KEEP(r_wrap(x));` with
  `r_obj* out = KEEP(rray_wrap_foreign(x, ownership));`.

- `ffi_rray_remove_axes()` passes `RRAY_OWNERSHIP_foreign`.

- `rray_unstack()` passes `RRAY_OWNERSHIP_owned`. Each element comes fresh
  from `rray_split()`, and only the `out` list refers to it.

## `rray_set_axis_names()`

- Add an `enum rray_ownership ownership` parameter after `names`, in
  `src/names.c` and `src/names.h`.

- Replace `r_obj* out = KEEP(r_wrap(x));` with
  `r_obj* out = KEEP(rray_wrap_foreign(x, ownership));`.

- `ffi_rray_set_axis_names()`, `ffi_rray_set_row_names()`, and
  `ffi_rray_set_column_names()` pass `RRAY_OWNERSHIP_foreign`.

- `rray_stack()` passes `RRAY_OWNERSHIP_owned`. Its `out` comes fresh from
  `rray_combine()`.

## Why these two are safe

Poking `dim` onto an owned array goes through R's `dimgets()`, which checks
the length and drops old `dimnames`, exactly as it does on the wrapper today.
The values are the same as before; only the wrapper is gone.

`arg_as_array()` at the top of both functions returns `x` unchanged here,
because both inputs already have dimensions.

## Tests

In `tests/testthat/test-unstack.R`:

```r
test_that("results are not wrappers", {
  x <- array(1:6, c(2L, 3L))
  out <- rray_unstack(x, axis = 2L)
  expect_identical(is_wrapper(out[[1]]), FALSE)
})
```

In `tests/testthat/test-stack.R`, the same for `rray_stack(x, x, .axis = 1L)`
with named inputs (so the `rray_set_axis_names()` path runs), and one case
without names.

`is_wrapper()` is defined in `helper-wrapper.R`. testthat loads every helper
file, so it is available in these test files.

---

# Part 6: Checks for every pull request

Run these before calling a pull request done.

1. `clang-format -i src/*.c src/*.h`, over all files.

2. `air format .`

3. `Rscript -e "devtools::test()"`, all green.

4. The protection pass from AGENTS.md, as a separate step: for every new or
   touched `r_obj*` in the diff, name the function it is next passed to, and
   check whether that function allocates before it protects or consumes the
   value. In particular:

   - every `r_unwrap()` result is `KEEP()`ed before the next allocation,

   - every changed `FREE()` count matches the new `KEEP()`s on every path,
     including early returns (`RRAY_LOCATE()` returns before its new
     `KEEP()`, `rray_roll_each_normalize()` and the reduce kernels return
     from two places), and in the `void` fill macros, which had no `FREE()`
     before,

   - every `x_data` is created on the line before its `r_*_cbegin()` and
     never passed to another function (rule 4 in Part 2),

   - `KEEP_N()` counters include the new protections.

5. The `grep` from Part 3, with every hit accounted for.

6. No new comments anywhere in `src/` or `tests/`.

7. For pull request 2, the sweep test passes and every stale comment is
   listed in the description.

8. `Rscript -e "devtools::check()"` once the stack is complete.

Use `/gh-stack` to create and push the three branches.

---

# Part 7: Costs and alternatives

## Costs that remain

- A base R or third party read of an rray result with shared data copies it
  once. For example `rray_set_dimensions(x, d) + 1` copies `x`, then locks
  the wrapper. Base R's own wrapper now pays the same cost.

- Rewrapping a locked wrapper copies. A locked wrapper only exists after
  non-rray code took a pointer from it, so this happens when a user mixes
  base R reads with rray attribute functions on the same object.

## Alternatives considered

- Keep the old data alive instead of locking (stash it in `data2`). This
  fixes the use after free but not the stale pointer, and it holds two copies
  in memory.

- No lock, and share data on rewrap only when it is already
  `MAYBE_SHARED()`. Safe, but a refcount of 1 cannot tell "a pointer is out"
  from "nobody has looked yet", so every rewrap of a temporary would copy.
  The lock records exactly that one bit.

- Copy in `Dataptr_or_null` like base R. Rejected above: it turns cheap
  region reads into copies.

- Refuse writable pointers for every type. This makes the bug impossible, but
  breaks in-place writes such as `[<-` on a wrapper.
