#' Generate filters definition based on the Source data
#'
#' The method should analyze source data structure, generate proper filters based on
#' the data (e.g. column types) and attach them to source.
#'
#' @param source Source object.
#' @param attach_as Choose whether the filters should be attached as a new step, or list of available filters (used in filtering panel when `new_step = "configure"`). By default in \code{step}.
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


#' Generate available filters choices based on the Source data
#'
#' The method should return the available choices for
#' virtualSelect input.
#'
#' @param source Source object.
#' @param cohort cohortBuilder cohort object
#' @param ... Extra arguments passed to a specific method.
#' @export
available_filters_choices <- function(source, cohort, ...) {
  UseMethod("available_filters_choices", source)
}

#' @rdname available_filters_choices
#' @export
available_filters_choises.default <- function(source, cohort, ...) {
  stop("The `available_filters` method is missing for the used Source type.")
}
