# setting an element of a character or list view errors

    Code
      view_set_elt(view(c("a", "b")))
    Condition
      Error in `view_set_elt()`:
      ! Can't set an element of a character view.
    Code
      view_set_elt(view(list(1, 2)))
    Condition
      Error in `view_set_elt()`:
      ! Can't set an element of a list view.

