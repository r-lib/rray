# Cannot use vec_restore() right now because it tries to restore the size
# for arrays and matrices and this is incorrect. It also does not correctly
# handle dimension names / names, I think it should just remove them.

# rray_restore() is useful for restoring the entire type EXCEPT
# - no names are restored
# - the size comes from x
# - the shape comes from to

rray_restore <- function(x, to) {
  UseMethod("rray_restore", to)
}

rray_restore.default <- function(x, to) {

  shape <- rray_shape(to)
  size <- vec_size(x)
  dims <- vec_dims(to)

  restore_attr <- most_attributes(to)
  restore_attr_nms <- names(restore_attr)

  if ("dim" %in% restore_attr_nms) {
    restore_attr$dim <- c(size, shape)
  }

  attributes(x) <- restore_attr

  x
}

# rray_partial_restore() is useful for restoring the entire type EXCEPT
# - no names are restored
# - the size comes from x
# - the shape comes from x
# (commonly used in broadcasting where size and shape changes
#  but the class is the same)

rray_partial_restore <- function(x, to) {

  # Restore all attributes except names
  restore_attr <- most_attributes(to)

  # The dimension of x is kept (size and shape)
  # If x has a `dim` attribute, we want to keep it
  # otherwise remove it from the attribs of `to`
  if ("dim" %in% names(attributes(x))) {
    restore_attr[["dim"]] <- vec_dim(x)
  }
  else {
    restore_attr[["dim"]] <- NULL
  }

  attributes(x) <- restore_attr

  x
}

# Retrieve all attributes except names/dimnames
# (return an empty list if no attributes because we have to assign `dim` to it)
most_attributes <- function(x) {
  x_attr <- attributes(x) %||% list()

  x_attr[["dimnames"]] <- NULL
  x_attr[["names"]] <- NULL

  x_attr
}
