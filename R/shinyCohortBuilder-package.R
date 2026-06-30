#' GUI layer for cohortBuilder package
#'
#' @name shinyCohortBuilder-package
#' @importFrom dplyr sym
#' @importFrom cohortBuilder get_filter_params

globalVariables(c(
  ":=", "!!", ".data", ".bin",
  "action", "count", "dataset", "id",
  "l_bound", "level", "line_id", "n", "name", "params",
  "patient_id", "state", "value", "variable"
))

NULL

#' Touch `htmltools` so it is treated as an imported dependency
#'
#' Never called; references `htmltools::tag` purely so `R CMD check` registers
#' the import and does not flag the package as unused.
#'
#' @return Not meaningful; not intended to be called.
#' @noRd
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
#'   \item{\code{.render_filters}}
#'   \item{\code{.update_data_stats}}
#'   \item{\code{.step_attrition}}
#'   \item{\code{.custom_attrition}}
#'   \item{\code{.available_filter_choices}}
#'   \item{\code{autofilter}}
#' }
#' Except from the above methods, you may extend the existing or new source with providing
#' custom gui filtering methods. See \link{gui-filter-layer}.
#' In order to see more details about how to implement custom source check `vignette("custom-gui-layer")`.
#'
#' @return Various type outputs dependent on the selected method.
#'   See each method documentation for details.
#' @name source-gui-layer
NULL

#' Access an internal (non-exported) object from another package
#' @param pkg Package name (unquoted).
#' @param name Object name (unquoted).
#' @return The object from the package's namespace.
#' @noRd
`%:::%` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}
