# FAQ - How are common names determined?

When a function builds one result from several inputs, the inputs must
agree on their names. If they don’t, the function errors instead of
silently picking one input’s names.

These functions follow the rules:

- [`rray_add()`](https://rray.r-lib.org/reference/rray-arithmetic.md),
  [`rray_subtract()`](https://rray.r-lib.org/reference/rray-arithmetic.md),
  [`rray_multiply()`](https://rray.r-lib.org/reference/rray-arithmetic.md),
  [`rray_divide()`](https://rray.r-lib.org/reference/rray-arithmetic.md),
  and
  [`rray_exponentiate()`](https://rray.r-lib.org/reference/rray-arithmetic.md)

- [`rray_and()`](https://rray.r-lib.org/reference/rray-logical.md),
  [`rray_or()`](https://rray.r-lib.org/reference/rray-logical.md), and
  [`rray_xor()`](https://rray.r-lib.org/reference/rray-logical.md)

- [`rray_pmax()`](https://rray.r-lib.org/reference/rray-extremum.md) and
  [`rray_pmin()`](https://rray.r-lib.org/reference/rray-extremum.md)

- [`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md)
  and [`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md)

## Rules

For each axis of the result:

All inputs with names along that axis must have exactly the same names.
If all names are the same, the result has those names. If no input is
named, the result is not named. If any names are different, an error is
thrown to avoid arbitrarily picking between the inputs. If an input is
broadcast or has no names, it is excluded from common name
determination.

To resolve a conflict, preemptively remove the names from one input with
[`rray_set_names()`](https://rray.r-lib.org/reference/rray_set_names.md).

## Examples

Matching names are kept, and different names are an error:

    x <- array(1:2, 2, dimnames = list(c("r1", "r2")))
    y <- array(1:2, 2, dimnames = list(c("a", "b")))

    rray_add(x, x)
    #> r1 r2
    #>  2  4

    rray_add(x, y)
    #> Error in `rray_add()`:
    #> ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "r1" and "a".

    rray_add(x, rray_set_names(y, NULL))
    #> r1 r2
    #>  2  4

An input without names on an axis doesn’t participate there. Here, each
input names a different axis, so both sets of names are kept:

    x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), NULL))
    y <- array(5:8, c(2, 2), dimnames = list(NULL, c("c1", "c2")))

    rray_add(x, y)
    #>    c1 c2
    #> r1  6 10
    #> r2  8 12

A broadcast axis doesn’t participate either. `"total"` names a single
row that gets repeated, so it no longer describes the result and is
dropped:

    x <- array(1:3, c(1, 3), dimnames = list("total", c("a", "b", "c")))
    y <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("a", "b", "c")))

    rray_multiply(x, y)
    #>    a b  c
    #> r1 1 6 15
    #> r2 2 8 18

Empty names count, so `c("r1", "")` and `c("r1", "r2")` are different:

    x <- array(1:2, 2, dimnames = list(c("r1", "")))
    y <- array(1:2, 2, dimnames = list(c("r1", "r2")))

    rray_add(x, y)
    #> Error in `rray_add()`:
    #> ! Can't find common names at axis 1. `x` and `y` have different names at position 2: "" and "r2".

## Combining and stacking

[`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md)
never checks names on `.axis`. They are combined from every input, with
`""` for inputs that have none. Every other axis follows the rules
above:

    a <- array(1:2, c(1, 2), dimnames = list("r1", c("x", "y")))
    b <- array(3:4, c(1, 2))
    c <- array(5:6, c(1, 2), dimnames = list("r3", c("x", "z")))

    rray_combine(a, b, c, .axis = 1)
    #> Error in `rray_combine()`:
    #> ! Can't find common names at axis 2. `..1` and `..3` have different names at position 2: "y" and "z".

    c <- array(5:6, c(1, 2), dimnames = list("r3", c("x", "y")))

    rray_combine(a, b, c, .axis = 1)
    #>    x y
    #> r1 1 2
    #>    3 4
    #> r3 5 6

[`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md) names
the new axis with the names of `...`. Every other axis follows the rules
above, which catches data that was reordered:

    jan <- array(1:2, 2, dimnames = list(c("north", "south")))
    feb <- array(3:4, 2, dimnames = list(c("south", "north")))

    rray_stack(jan = jan, feb = feb, .axis = 2)
    #> Error in `rray_stack()`:
    #> ! Can't find common names at axis 1. `jan` and `feb` have different names at position 1: "north" and "south".

## Comparisons

Comparison functions like
[`rray_equal()`](https://rray.r-lib.org/reference/rray-equal.md) and
[`rray_greater_than()`](https://rray.r-lib.org/reference/rray-compare.md)
return a logical relation about their inputs rather than a new value of
the input type, so they don’t return names on the result and aren’t
subject to these checks:

    x <- array(c(TRUE, FALSE), 2, dimnames = list(c("r1", "r2")))
    y <- array(c(TRUE, TRUE), 2, dimnames = list(c("a", "b")))

    rray_equal(x, y)
    #> [1]  TRUE FALSE
