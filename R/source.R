#' Create target filter position in UI panel
#'
#' @param Source Source object.
#' @param step_id Id of the step that filter belong to.
#' @param name filter Filter object.
#' @param ... Extra arguments passed to related method.
#' @name filter-position
#' @export
.filter_position <- function(source, step_id, filter, ...) {
  UseMethod(".filter_position", source)
}

#' @rdname filter-position
#' @export
.filter_position.default <- function(source, step_id, filter, ...) {
  return(step_id)
}
