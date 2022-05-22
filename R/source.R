#' Generate filters definition based on the Source data
#'
#' The method should analyze source data structure, generate proper filters based on
#' the data (e.g. column types) and attach them to source.
#'
#' @param source Source object.
#' @param ... Extra arguments passed to a specific method.
#' @seealso \link{source-gui-layer}
#' @export
autofilter <- function(source, ...) {
  UseMethod("autofilter", source)
}

#' @rdname autofilter
#' @export
autofilter.default <- function(source, ...) {
  return(source)
}
