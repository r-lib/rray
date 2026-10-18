# Titles still to handle

The single-array functions keep titles as of #154 to #159. The functions below
still drop them. They all combine names across several inputs, so they get
titles when we finalize how dimension names combine (see `plans/n-ary.md`).

Each one was checked against:

```r
x <- array(1:24, c(2, 3, 4), dimnames = list(A = c("a", "b"), B = NULL, C = NULL))
```

## Broadcasting

- `rray_broadcast()`: drops titles whenever it actually broadcasts. It keeps
  them only on a no-op, where `x` comes back untouched. The `"dimension titles
  are lost"` test in `test-broadcast.R` pins this down today.

- `rray_broadcast_common()`: the same, per input. An input that already has
  the common dimensions keeps its titles, any other input loses them.

Both go through `rray_broadcast_names()` in `src/broadcast-names.c`.

## Arithmetic

Built on `rray_broadcast_names2()` in `src/arithmetic.c`. Titles are dropped
even for `rray_add(x, x)`, where nothing broadcasts.

- `rray_add()`

- `rray_subtract()`

- `rray_multiply()`

- `rray_divide()`

- `rray_exponentiate()`

## Comparison

Built on `rray_broadcast_names2()` in `src/compare.c`.

- `rray_greater_than()`, `rray_greater_than_or_equal()`

- `rray_less_than()`, `rray_less_than_or_equal()`

- `rray_not_equal()`

- `rray_equal()`, in `src/equal.c`

## Logical

Built on `rray_broadcast_names2()` in `src/logical.c`.

- `rray_and()`

- `rray_or()`

- `rray_xor()`

## Parallel extremum

Built on `rray_broadcast_names2()` in `src/extremum.c`.

- `rray_pmax()`

- `rray_pmin()`

## Combining

Built on `rray_broadcast_names_common_opts()` in `src/combine.c`. Titles are
dropped even with a single input.

- `rray_combine()`

- `rray_stack()`, which also goes through the internal
  `rray_expand_dimensionality()` in `src/dimensionality.c`. The new axis should
  get the title `""`.

## Not on this list

These drop titles on purpose, because they drop all names. They are pinned by
tests and need no further work:

- `rray_extract()`

- `rray_index()`

- `rray_set_dimensions()`

- `rray_if_else()`
