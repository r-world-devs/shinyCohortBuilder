button <- function(..., icon = NULL, type = getOption("scb_button_type", "btn-default btn-outline-dark")) {
  shiny::tags$button(type = "button", class = paste("scb_button btn", type), icon, ...)
}

panel <- function(heading, body, ...) {
  shiny::div(
    class = "panel panel-default card",
    if (!missing(heading)) {
      shiny::div(
        class = "panel-heading card-header",
        heading
      )
    },
    shiny::div(
      class = "panel-body card-body",
      body
    ),
    ...
  )
}

divider <- function(label) {
  shiny::div(
    class = "divider",
    shiny::hr(style = "float:left;"),
    label,
    shiny::hr(style = "float:right;")
  )
}

filter_help_icon <- function(filter, ns, method, description, cohort) {
  # todo move the check outside
  if (!isTRUE(cohort$attributes$show_help)) return(NULL)
  if (is.null(description)) return(NULL)

  shiny::a(
    href = "#",
    class = "filter_tooltip",
    shiny::icon(
      "question-circle",
      onclick = .trigger_action_js("show_help", list(step_id = filter$step_id, filter_id = filter$id), ns = ns)
    )
  )
}

#' Default filtering panel labels
#'
#' Labels can be overwritten with using \code{sbc_labels} option.
#'
#' @export
scb_labels <- list(
  "run_steps_global" = "Run All Steps",
  "get_state" = "Get State",
  "set_state" = "Set State",
  "show_attrition" = "Show Attrition Data",
  "show_repro_code" = "Show Reproducible Code",
  "add_step" = "Add Step",
  "delete_step_title" = "Delete Step",
  "clear_filters_title" = "Clear Filters",
  "run_single_step_title" = "Run",
  "show_edit_title" = "Show / Edit",
  "keep_missing" = "Keep missing values",
  "step" = "Step"
)

#' Default color palette used for filter feedback plots
#'
#' It's a list of the following elements:
#'
#' \itemize{
#'   \item{\code{discrete}}{ Discrete filter plot colors.}
#'   \item{\code{histogram}}{ Range and date range histogram color.}
#'   \item{\code{no_data}}{ Color used to mark missing variables on feedback plots.}
#' }
#'
#' The palette is used as default \code{scb_chart_palette} option, that can be overwritten with custom palettes.
#'
#' @export
scb_chart_palette <- list(
  discrete = c(
    "#51a2e5", "#ff6696", "#d1a4d9", "#4cc8a4", "#ffde7e", "#ffbc62",
    "#bd6400", "#c49300", "#007342", "#80428b", "#ba0031", "#004c9e"
  ),
  histogram = "#51a2e5",
  no_data = "grey40"
)
