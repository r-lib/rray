# Package index

## Elementwise

- [`rray_add()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
  [`rray_subtract()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
  [`rray_multiply()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
  [`rray_divide()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
  [`rray_exponentiate()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
  : Array arithmetic
- [`rray_pmax()`](https://rray.r-lib.org/reference/rray-extremum.md)
  [`rray_pmin()`](https://rray.r-lib.org/reference/rray-extremum.md) :
  Elementwise maximum and minimum
- [`rray_equal()`](https://rray.r-lib.org/reference/rray-equal.md)
  [`rray_not_equal()`](https://rray.r-lib.org/reference/rray-equal.md) :
  Equality
- [`rray_greater_than()`](https://rray.r-lib.org/reference/rray-compare.md)
  [`rray_greater_than_or_equal()`](https://rray.r-lib.org/reference/rray-compare.md)
  [`rray_less_than()`](https://rray.r-lib.org/reference/rray-compare.md)
  [`rray_less_than_or_equal()`](https://rray.r-lib.org/reference/rray-compare.md)
  : Compare arrays
- [`rray_and()`](https://rray.r-lib.org/reference/rray-logical.md)
  [`rray_or()`](https://rray.r-lib.org/reference/rray-logical.md)
  [`rray_xor()`](https://rray.r-lib.org/reference/rray-logical.md) :
  Logical operations
- [`rray_if_else()`](https://rray.r-lib.org/reference/rray_if_else.md) :
  If-else for arrays

## Reducers

- [`rray_sum()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_prod()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_mean()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_all()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_any()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_max()`](https://rray.r-lib.org/reference/rray-reduce.md)
  [`rray_min()`](https://rray.r-lib.org/reference/rray-reduce.md) :
  Reduce an array along axes
- [`rray_locate_max()`](https://rray.r-lib.org/reference/rray-locate.md)
  [`rray_locate_min()`](https://rray.r-lib.org/reference/rray-locate.md)
  : Locate the maximum or minimum along an axis

## Combining and Splitting

- [`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md) :
  Combine arrays along an existing axis
- [`rray_split()`](https://rray.r-lib.org/reference/rray_split.md) :
  Split an array along an axis
- [`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md) :
  Stack arrays along a new axis
- [`rray_unstack()`](https://rray.r-lib.org/reference/rray_unstack.md) :
  Unstack an array

## Indexing

- [`rray_slice()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_axis()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_rows()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_columns()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_assign()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_assign_axis()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_assign_rows()`](https://rray.r-lib.org/reference/rray_slice.md)
  [`rray_slice_assign_columns()`](https://rray.r-lib.org/reference/rray_slice.md)
  : Slice an array
- [`rray_index()`](https://rray.r-lib.org/reference/rray_index.md) :
  Index an array by coordinates
- [`rray_extract()`](https://rray.r-lib.org/reference/rray_extract.md)
  [`rray_extract_assign()`](https://rray.r-lib.org/reference/rray_extract.md)
  : Extract values from an array
- [`rray_rep()`](https://rray.r-lib.org/reference/rray-rep.md)
  [`rray_rep_each()`](https://rray.r-lib.org/reference/rray-rep.md) :
  Repeat an array
- [`rray_roll()`](https://rray.r-lib.org/reference/rray-roll.md)
  [`rray_roll_each()`](https://rray.r-lib.org/reference/rray-roll.md) :
  Roll an array
- [`rray_reverse()`](https://rray.r-lib.org/reference/rray_reverse.md) :
  Reverse elements along axes

## Broadcasting

- [`rray_broadcast()`](https://rray.r-lib.org/reference/rray_broadcast.md)
  : Broadcast an array to new dimensions
- [`rray_broadcast_common()`](https://rray.r-lib.org/reference/rray_broadcast_common.md)
  : Broadcast arrays to common dimensions

## Dimensions

- [`rray_dimensions()`](https://rray.r-lib.org/reference/rray_dimensions.md)
  : Get the dimensions of an array
- [`rray_dimensions_common()`](https://rray.r-lib.org/reference/rray_dimensions_common.md)
  : Find common dimensions
- [`rray_dimensionality()`](https://rray.r-lib.org/reference/rray_dimensionality.md)
  : Find the dimensionality of an array
- [`rray_size()`](https://rray.r-lib.org/reference/rray_size.md) : Find
  the size of an array
- [`rray_set_dimensions()`](https://rray.r-lib.org/reference/rray_set_dimensions.md)
  : Set the dimensions of an array

## Axes

- [`rray_permute_axes()`](https://rray.r-lib.org/reference/rray_permute_axes.md)
  : Permute array axes
- [`rray_move_axes()`](https://rray.r-lib.org/reference/rray_move_axes.md)
  : Move array axes
- [`rray_insert_axes()`](https://rray.r-lib.org/reference/rray_insert_axes.md)
  : Insert array axes
- [`rray_remove_axes()`](https://rray.r-lib.org/reference/rray_remove_axes.md)
  : Remove array axes

## Names

- [`rray_names()`](https://rray.r-lib.org/reference/rray_names.md) : Get
  names for each axis of an array
- [`rray_axis_names()`](https://rray.r-lib.org/reference/rray_axis_names.md)
  [`rray_row_names()`](https://rray.r-lib.org/reference/rray_axis_names.md)
  [`rray_column_names()`](https://rray.r-lib.org/reference/rray_axis_names.md)
  : Get names for a single axis of an array
- [`rray_set_names()`](https://rray.r-lib.org/reference/rray_set_names.md)
  : Set names for every axis of an array
- [`rray_set_axis_names()`](https://rray.r-lib.org/reference/rray_set_axis_names.md)
  [`rray_set_row_names()`](https://rray.r-lib.org/reference/rray_set_axis_names.md)
  [`rray_set_column_names()`](https://rray.r-lib.org/reference/rray_set_axis_names.md)
  : Set names for a single axis of an array
