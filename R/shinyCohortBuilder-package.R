#' GUI layer for cohortBuilder package
#'
#' @name shinyCohortBuilder-package
#' @importFrom magrittr %>%
#' @importFrom dplyr sym

globalVariables(c(
  ":=", "!!", ".data",
  "action", "count", "dataset", "id",
  "l_bound", "level", "line_id", "n", "name", "params",
  "patient_id", "state", "value", "variable"
))

NULL

force_import <- function() {
  htmltools::tag
}

#' Source compatibility methods.
#'
#' @description
#' List of methods that allow compatibility of different source types.
#' Most of the methods should be defined in order to make new source layer functioning.
#' See 'Details' section for more information.
#'
#' @details
#' The package is designed to make the functionality work with multiple data sources.
#' Data source can be based for example on list of tables, connection to database schema
#' or API service that allows to access and operate on data.
#' In order to make new source type layer functioning, the following list of methods
#' should be defined:
#' \itemize{
#'   \item{.render_filters}{ }
#'   \item{.update_data_stats}{ }
#'   \item{.step_attrition}{ }
#'   \item{.custom_attrition}{ }
#'   \item{.available_filter_choices}{ }
#'   \item{autofilter}{ }
#' }
#' Except from the above methods, you may extend the existing or new source with providing
#' custom gui filtering methods. See \link{gui-filter-layer}.
#' In order to see more details about how to implement custom source check `vignette("custom-gui-layer")`.
#'
#' @return Various type outputs dependent on the selected method.
#'   See each method documentation for details.
#' @name source-gui-layer
NULL

`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
