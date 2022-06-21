#' Generate filters definition based on the Source data
#'
#' The method should analyze source data structure, generate proper filters based on
#' the data (e.g. column types) and attach them to source.
#'
#' @param source Source object.
#' @param attach_as Choose whether the filters should be attached as a new step,
#'    or list of available filters (used in filtering panel when `new_step = "configure"`).
#'    By default in \code{step}.
#' @param ... Extra arguments passed to a specific method.
#' @return Source object having step configuration attached.
#' @seealso \link{source-gui-layer}
#'
#' @examples
#' library(magrittr)
#' library(cohortBuilder)
#' library(shinyCohortBuilder)
#'
#' iris_source <- set_source(tblist(iris = iris)) %>%
#'   autofilter()
#' iris_cohort <- cohort(iris_source)
#' sum_up(iris_cohort)
#'
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     cb_ui("mycoh")
#'   )
#'
#'   server <- function(input, output, session) {
#'     cb_server("mycoh", cohort = iris_cohort)
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
autofilter <- function(source, attach_as = c("step", "meta"), ...) {
  UseMethod("autofilter", source)
}

#' @rdname autofilter
#' @export
autofilter.default <- function(source, ...) {
  return(source)
}
