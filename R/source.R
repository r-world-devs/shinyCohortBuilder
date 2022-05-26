#' Generate filters definition based on the Source data
#'
#' The method should analyze source data structure, generate proper filters based on
#' the data (e.g. column types) and attach them to source.
#'
#' @param source Source object.
#' @param attach_as Chose where the filters should be stored. By default in \code{step}
#' @param ... Extra arguments passed to a specific method.
#' @seealso \link{source-gui-layer}
#' @export
autofilter <- function(source, attach_as = c("step", "meta"), ...) {
  UseMethod("autofilter", source)
}

#' @rdname autofilter
#' @export
autofilter.default <- function(source, ...) {
  return(source)
}
