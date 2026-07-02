#' Create target filter position in UI panel
#'
#' @param source Source object.
#' @param step_id Id of the step that filter belong to.
#' @param filter Filter object.
#' @param ns Namespace function.
#' @param ... Extra arguments passed to related method.
#' @name filter-position
#' @export
.filter_position <- function(source, step_id, filter, ns, ...) {
  UseMethod(".filter_position", source)
}

#' @rdname filter-position
#' @export
.filter_position.default <- function(source, step_id, filter, ns, ...) {
  return(step_id)
}
