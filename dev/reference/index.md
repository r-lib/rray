# Package index

## Elementwise

- [`rray_add()`](https://r-lib.github.io/rray/dev/reference/rray-arithmetic.md)
  [`rray_subtract()`](https://r-lib.github.io/rray/dev/reference/rray-arithmetic.md)
  [`rray_multiply()`](https://r-lib.github.io/rray/dev/reference/rray-arithmetic.md)
  [`rray_divide()`](https://r-lib.github.io/rray/dev/reference/rray-arithmetic.md)
  [`rray_exponentiate()`](https://r-lib.github.io/rray/dev/reference/rray-arithmetic.md)
  : Array arithmetic
- [`rray_pmax()`](https://r-lib.github.io/rray/dev/reference/rray-extremum.md)
  [`rray_pmin()`](https://r-lib.github.io/rray/dev/reference/rray-extremum.md)
  : Elementwise maximum and minimum
- [`rray_equal()`](https://r-lib.github.io/rray/dev/reference/rray-equal.md)
  [`rray_not_equal()`](https://r-lib.github.io/rray/dev/reference/rray-equal.md)
  : Equality
- [`rray_greater_than()`](https://r-lib.github.io/rray/dev/reference/rray-compare.md)
  [`rray_greater_than_or_equal()`](https://r-lib.github.io/rray/dev/reference/rray-compare.md)
  [`rray_less_than()`](https://r-lib.github.io/rray/dev/reference/rray-compare.md)
  [`rray_less_than_or_equal()`](https://r-lib.github.io/rray/dev/reference/rray-compare.md)
  : Compare arrays
- [`rray_and()`](https://r-lib.github.io/rray/dev/reference/rray-logical.md)
  [`rray_or()`](https://r-lib.github.io/rray/dev/reference/rray-logical.md)
  [`rray_xor()`](https://r-lib.github.io/rray/dev/reference/rray-logical.md)
  : Logical operations
- [`rray_if_else()`](https://r-lib.github.io/rray/dev/reference/rray_if_else.md)
  : If-else for arrays

## Reducers

- [`rray_sum()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_prod()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_mean()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_all()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_any()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_max()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  [`rray_min()`](https://r-lib.github.io/rray/dev/reference/rray-reduce.md)
  : Reduce an array along axes
- [`rray_locate_max()`](https://r-lib.github.io/rray/dev/reference/rray-locate.md)
  [`rray_locate_min()`](https://r-lib.github.io/rray/dev/reference/rray-locate.md)
  : Locate the maximum or minimum along an axis

## Cumulative

- [`rray_cumulative_all()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  [`rray_cumulative_any()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  [`rray_cumulative_max()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  [`rray_cumulative_min()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  [`rray_cumulative_prod()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  [`rray_cumulative_sum()`](https://r-lib.github.io/rray/dev/reference/rray-cumulative.md)
  : Accumulate along an axis

## Combining and Splitting

- [`rray_combine()`](https://r-lib.github.io/rray/dev/reference/rray_combine.md)
  : Combine arrays along an existing axis
- [`rray_split()`](https://r-lib.github.io/rray/dev/reference/rray_split.md)
  : Split an array along an axis
- [`rray_stack()`](https://r-lib.github.io/rray/dev/reference/rray_stack.md)
  : Stack arrays along a new axis
- [`rray_unstack()`](https://r-lib.github.io/rray/dev/reference/rray_unstack.md)
  : Unstack an array

## Indexing

- [`rray_slice()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_axis()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_rows()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_columns()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_assign()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_assign_axis()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_assign_rows()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  [`rray_slice_assign_columns()`](https://r-lib.github.io/rray/dev/reference/rray_slice.md)
  : Slice an array
- [`rray_index()`](https://r-lib.github.io/rray/dev/reference/rray_index.md)
  : Index an array by coordinates
- [`rray_extract()`](https://r-lib.github.io/rray/dev/reference/rray_extract.md)
  [`rray_extract_assign()`](https://r-lib.github.io/rray/dev/reference/rray_extract.md)
  : Extract values from an array
- [`rray_rep()`](https://r-lib.github.io/rray/dev/reference/rray-rep.md)
  [`rray_rep_each()`](https://r-lib.github.io/rray/dev/reference/rray-rep.md)
  : Repeat an array
- [`rray_roll()`](https://r-lib.github.io/rray/dev/reference/rray-roll.md)
  [`rray_roll_each()`](https://r-lib.github.io/rray/dev/reference/rray-roll.md)
  : Roll an array
- [`rray_reverse()`](https://r-lib.github.io/rray/dev/reference/rray_reverse.md)
  : Reverse elements along axes

## Broadcasting

- [`rray_broadcast()`](https://r-lib.github.io/rray/dev/reference/rray_broadcast.md)
  : Broadcast an array to new dimensions
- [`rray_broadcast_common()`](https://r-lib.github.io/rray/dev/reference/rray_broadcast_common.md)
  : Broadcast arrays to common dimensions

## Dimensions

- [`rray_dimensions()`](https://r-lib.github.io/rray/dev/reference/rray_dimensions.md)
  : Get the dimensions of an array
- [`rray_dimensions_common()`](https://r-lib.github.io/rray/dev/reference/rray_dimensions_common.md)
  : Find common dimensions
- [`rray_dimensionality()`](https://r-lib.github.io/rray/dev/reference/rray_dimensionality.md)
  : Find the dimensionality of an array
- [`rray_size()`](https://r-lib.github.io/rray/dev/reference/rray_size.md)
  : Find the size of an array
- [`rray_set_dimensions()`](https://r-lib.github.io/rray/dev/reference/rray_set_dimensions.md)
  : Set the dimensions of an array

## Axes

- [`rray_permute_axes()`](https://r-lib.github.io/rray/dev/reference/rray_permute_axes.md)
  : Permute array axes
- [`rray_move_axes()`](https://r-lib.github.io/rray/dev/reference/rray_move_axes.md)
  : Move array axes
- [`rray_insert_axes()`](https://r-lib.github.io/rray/dev/reference/rray_insert_axes.md)
  : Insert array axes
- [`rray_remove_axes()`](https://r-lib.github.io/rray/dev/reference/rray_remove_axes.md)
  : Remove array axes

## Names

- [`rray_names()`](https://r-lib.github.io/rray/dev/reference/rray_names.md)
  : Get names for each axis of an array
- [`rray_axis_names()`](https://r-lib.github.io/rray/dev/reference/rray_axis_names.md)
  [`rray_row_names()`](https://r-lib.github.io/rray/dev/reference/rray_axis_names.md)
  [`rray_column_names()`](https://r-lib.github.io/rray/dev/reference/rray_axis_names.md)
  : Get names for a single axis of an array
- [`rray_set_names()`](https://r-lib.github.io/rray/dev/reference/rray_set_names.md)
  : Set names for every axis of an array
- [`rray_set_axis_names()`](https://r-lib.github.io/rray/dev/reference/rray_set_axis_names.md)
  [`rray_set_row_names()`](https://r-lib.github.io/rray/dev/reference/rray_set_axis_names.md)
  [`rray_set_column_names()`](https://r-lib.github.io/rray/dev/reference/rray_set_axis_names.md)
  : Set names for a single axis of an array

## Titles

- [`rray_titles()`](https://r-lib.github.io/rray/dev/reference/rray-titles.md)
  [`rray_axis_title()`](https://r-lib.github.io/rray/dev/reference/rray-titles.md)
  [`rray_set_titles()`](https://r-lib.github.io/rray/dev/reference/rray-titles.md)
  [`rray_set_axis_title()`](https://r-lib.github.io/rray/dev/reference/rray-titles.md)
  : Array titles

## FAQ

- [`faq-common-names`](https://r-lib.github.io/rray/dev/reference/faq-common-names.md)
  : FAQ - How are common names and titles determined?
- [`faq-axes`](https://r-lib.github.io/rray/dev/reference/faq-axes.md) :
  FAQ - How do I refer to an axis?
