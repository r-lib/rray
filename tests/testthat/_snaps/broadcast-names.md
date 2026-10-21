# rray_broadcast_names2() errors on names that disagree

    Code
      rray_broadcast_names2(x, y, c(2L, 3L))
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common names at axis 2. `x` and `y` have different names at position 2: "c2" and "c9".

# rray_broadcast_names2() treats empty names like any other name

    Code
      rray_broadcast_names2(x, y, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common names at axis 1. `x` and `y` have different names at position 2: "" and "r2".

# rray_broadcast_names2() only matches `NA` with `NA`

    Code
      rray_broadcast_names2(x, y, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common names at axis 1. `x` and `y` have different names at position 2: `NA` and "NA".
    Code
      rray_broadcast_names2(y, x, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common names at axis 1. `x` and `y` have different names at position 2: "NA" and `NA`.

# rray_broadcast_names2() errors on titles that disagree

    Code
      rray_broadcast_names2(x, y, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common titles at axis 1. `x` has title "A" and `y` has title "B".

# rray_broadcast_names2() checks titles where an input broadcasts

    Code
      rray_broadcast_names2(x, y, c(2L, 2L))
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common titles at axis 1. `x` has title "A" and `y` has title "B".

# rray_broadcast_names2() only matches `NA` titles with `NA`

    Code
      rray_broadcast_names2(x, y, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common titles at axis 1. `x` has title `NA` and `y` has title "NA".
    Code
      rray_broadcast_names2(y, x, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common titles at axis 1. `x` has title "NA" and `y` has title `NA`.

# rray_broadcast_names2() checks names before titles

    Code
      rray_broadcast_names2(x, y, 2L)
    Condition
      Error in `rray_broadcast_names2()`:
      ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "a" and "c".

# rray_broadcast_names_common() errors on names that disagree

    Code
      rray_broadcast_names_common(x, y, .dimensions = c(2L, 3L))
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common names at axis 1. `..1` and `..2` have different names at position 1: "r1" and "a".
    Code
      rray_broadcast_names_common(x = x, y = y, .dimensions = c(2L, 3L))
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "r1" and "a".

# rray_broadcast_names_common() reports the input that set the names

    Code
      rray_broadcast_names_common(x, y, z, .dimensions = c(1L, 2L))
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common names at axis 2. `..1` and `..3` have different names at position 2: "b" and "c".

---

    Code
      rray_broadcast_names_common(x, y, z, .dimensions = c(2L, 2L))
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common names at axis 2. `..2` and `..3` have different names at position 2: "d" and "e".

# rray_broadcast_names_common() reports the input that set the title

    Code
      rray_broadcast_names_common(x, y, z, .dimensions = 2L)
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common titles at axis 1. `..1` has title "A" and `..3` has title "B".
    Code
      rray_broadcast_names_common(x = x, y = y, z = z, .dimensions = 2L)
    Condition
      Error in `rray_broadcast_names_common()`:
      ! Can't find common titles at axis 1. `x` has title "A" and `z` has title "B".

