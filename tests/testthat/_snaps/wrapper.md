# setting an element of a character or list wrapper errors

    Code
      wrapper_set_elt(wrap(c("a", "b")))
    Condition
      Error in `wrapper_set_elt()`:
      ! Can't set an element of a character wrapper.
    Code
      wrapper_set_elt(wrap(list(1, 2)))
    Condition
      Error in `wrapper_set_elt()`:
      ! Can't set an element of a list wrapper.

