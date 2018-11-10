#' @import nodegraph

new_delay_rray <- function(.data = numeric(), node, ..., subclass = character()) {
  nodegraph::new_delay_array(.data = .data, node = node, ..., subclass = c(subclass, "delay_rray"))
}


#' @export
delay_rray <- function(x = numeric()) {
  as_delay_rray(x)
}

#' @export
as_delay_rray <- function(x) {
  UseMethod("as_delay_rray")
}

#' @export
as_delay_rray.default <- function(x) {
  abort("Unknown input type. Cannot coerce to a delay_rray.")
}

#' @export
as_delay_rray.delay_rray <- function(x) {
  x
}

#' @export
as_delay_rray.numeric <- function(x) {
  as_delay_rray(ValueNode$new(value = x))
}

#' @export
as_delay_rray.logical <- function(x) {
  as_delay_rray(ValueNode$new(value = x))
}

#' @export
as_delay_rray.ValueNode <- function(x) {
  new_delay_rray(
    .data = NA, # mock something being here for vctrs
    node = x
  )
}

#' @export
print.delay_rray <- function(x, ...) {
  node <- get_node(x)
  cat("<delay_rray>")
  cat("\n")
  print(node$get_value(), ...)
}

#' @export
vec_restore.delay_rray <- function(x, to) {
  as_delay_rray(x)
}

# vec_type2 boilerplate --------------------------------------------------------

#' @export
#' @method vec_type2 delay_rray
#' @export vec_type2.delay_rray
vec_type2.delay_rray <- function(x, y) UseMethod("vec_type2.delay_rray")

#' @method vec_type2.delay_rray default
#' @export
vec_type2.delay_rray.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.delay_rray vctrs_unspecified
#' @export
vec_type2.delay_rray.vctrs_unspecified <- function(x, y) x

# vec_type2 delay_rray <-> delay_rray ------------------------------------------

#' @method vec_type2.delay_rray delay_rray
#' @export
vec_type2.delay_rray.delay_rray <- function(x, y) {
  new_delay_rray()
}

# vec_type2 delay_rray <-> double/matrix/array ---------------------------------

#' @method vec_type2.delay_rray double
#' @export
vec_type2.delay_rray.double <- vec_type2.delay_rray.delay_rray

#' @method vec_type2.double delay_rray
#' @export
vec_type2.double.delay_rray <- vec_type2.delay_rray.delay_rray

# vec_type2 delay_rray <-> integer/matrix/array --------------------------------

#' @method vec_type2.delay_rray integer
#' @export
vec_type2.delay_rray.integer <- vec_type2.delay_rray.delay_rray

#' @method vec_type2.integer delay_rray
#' @export
vec_type2.integer.delay_rray <- vec_type2.delay_rray.delay_rray

# vec_type2 delay_rray <-> delay_array --------------------------------

#' @method vec_type2.delay_rray delay_array
#' @export
vec_type2.delay_rray.delay_array <- vec_type2.delay_rray.delay_rray

#' @method vec_type2.delay_array delay_rray
#' @export
vec_type2.delay_array.delay_rray <- vec_type2.delay_rray.delay_rray

#' @export
#' @importFrom nodegraph compute_engine
compute_engine.delay_rray <- function(x, self) {

  children <- self$get_children()
  vals <- lapply(children, function(x) x$get_value())
  vals <- lapply(vals, as_rray)

  op <- self$get_operation()

  do.call(vec_arith, c(op = op, vals))
}

#' @export
#' @importFrom nodegraph compute_dim_engine
compute_dim_engine.delay_rray <- function(type, operation, ...) {
  delay_args <- list(...)
  nodes <- map(delay_args, get_node)
  dim_lst <- lapply(nodes, function(node) node$get_dim())
  # broadcasting!
  eval_bare(expr(pmax(!!!dim_lst)))
}
