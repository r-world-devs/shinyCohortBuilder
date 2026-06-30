#' Detect the active Bootstrap major version
#'
#' Reads the current (or global) bslib theme to determine the Bootstrap version,
#' defaulting to `"5"` when no theme is set. Used to emit version-appropriate
#' `data-*` attributes.
#'
#' @return The Bootstrap version as a character string (e.g. `"5"`).
#' @noRd
get_bs <- function() {
  theme <- shiny::getCurrentTheme()
  if (!bslib::is_bs_theme(theme)) {
    theme <- bslib::bs_global_get()
  }
  if (bslib::is_bs_theme(theme)) {
    bslib::theme_version(theme)
  } else {
    "5"
  }
}

#' Build a Bootstrap-version-aware `data-*` attribute
#'
#' Bootstrap 5 namespaces JS data attributes as `data-bs-*`; earlier versions use
#' `data-*`. Produces a named single-element list suitable for splicing into a tag.
#'
#' @param attr Attribute name without the `data-`/`data-bs-` prefix (e.g. `"toggle"`).
#' @param value Attribute value.
#' @return A named list of length one mapping the resolved attribute name to `value`.
#' @noRd
bs_data_attr <- function(attr, value) {
  bs <- get_bs()
  name <- if (bs >= "5") paste0("data-bs-", attr) else paste0("data-", attr)
  stats::setNames(list(value), name)
}

#' Render a styled toolbar button
#' @param ... Button label/content passed to `shiny::tags$button`.
#' @param icon Optional icon tag rendered before the content.
#' @param type Bootstrap button class (defaults to option `scb_button_type`).
#' @return A `<button>` `shiny.tag`.
#' @noRd
button <- function(..., icon = NULL, type = getOption("scb_button_type", "btn-outline-dark")) {
  shiny::tags$button(type = "button", class = paste("scb_button btn", type), icon, ...)
}

#' Render a Bootstrap card panel with optional header
#' @param heading Optional header content; omitted when missing.
#' @param body Card body content.
#' @param ... Extra elements appended to the card container.
#' @return A card `shiny.tag`.
#' @noRd
panel <- function(heading, body, ...) {
  shiny::div(
    class = "card",
    if (!missing(heading)) {
      shiny::div(
        class = "card-header",
        heading
      )
    },
    shiny::div(
      class = "card-body",
      body
    ),
    ...
  )
}

#' Render a labelled horizontal divider
#' @param label Text shown between the two rules.
#' @return A divider `shiny.tag`.
#' @noRd
divider <- function(label) {
  shiny::div(
    class = "divider",
    shiny::hr(style = "float:left;"),
    label,
    shiny::hr(style = "float:right;")
  )
}

#' Render a filter's help tooltip icon
#'
#' Returns the clickable help icon for a filter, or `NULL` when help is disabled
#' for the cohort or the filter has no description. Clicking triggers the
#' `show_help` GUI action.
#'
#' @param filter The cohortBuilder filter object.
#' @param ns Module namespace function.
#' @param method Filter rendering method (unused; kept for signature symmetry).
#' @param description Filter description; `NULL` suppresses the icon.
#' @param cohort The cohort (its `attributes$show_help` gates display).
#' @return An `<a>` `shiny.tag`, or `NULL`.
#' @noRd
filter_help_icon <- function(filter, ns, method, description, cohort) {
  # todo move the check outside
  if (!isTRUE(cohort$attributes$show_help)) return(NULL)
  if (is.null(description)) return(NULL)

  shiny::a(
    href = "#",
    class = "filter_tooltip",
    getOption("scb_icons", scb_icons)$filter_help |>
      shiny::tagAppendAttributes(
        onclick = .trigger_action_js("show_help", list(step_id = filter@step_id, filter_id = filter@id), ns = ns)
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
  "step" = "Step",
  "filter_discrete_text_bttn_label" = "Set Values",
  "filter_query_bttn_label" = "Set Query",
  "filter_show_query_bttn_label" = "Show Query",
  "manage_step" = "Manage Last Step",
  "show_assistant" = "Open Assistant"
)

#' Default filtering panel icons
#'
#' Icons can be overwritten with using \code{sbc_icons} option.
#'
#' @export
scb_icons <- list(
  "run_steps_global" = shiny::icon("play"),
  "get_state" = shiny::icon("sliders-h"),
  "set_state" = shiny::icon("stream"),
  "show_attrition" = shiny::icon("project-diagram"),
  "show_repro_code" = shiny::icon("code"),
  "add_step" = shiny::icon("plus"),
  "delete_step" = shiny::icon("trash-alt"),
  "clear_filters" = shiny::icon("sync-alt", class = "fa-flip-horizontal"),
  "run_single_step" = shiny::icon("play"),
  "show_edit" = shiny::icon("eye"),
  "filter_help" = shiny::icon("question-circle"),
  "filter_discrete_text_bttn_icon" = shiny::icon("keyboard"),
  "filter_query_bttn_icon" = shiny::icon("arrow-pointer"),
  "filter_show_query_bttn_icon" = shiny::icon("eye"),
  "dataset_help_icon" = shiny::icon("question-circle"),
  "manage_step" = shiny::icon("pen-to-square"),
  "show_assistant" = shiny::icon("user")
)

#' Default color palette used for filter feedback plots
#'
#' It's a list of the following elements:
#'
#' \itemize{
#'   \item{\code{discrete} - Discrete filter plot colors.}
#'   \item{\code{histogram} - Range and date range histogram color.}
#'   \item{\code{no_data} - Color used to mark missing variables on feedback plots.}
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

#' Track changes of cohort data in Shiny
#'
#' The function returns Shiny input object related to selected cohort that is triggered whenever
#' cohort data filters were applied to it within filtering panel.
#'
#' The function is meant to be used as a trigger for Shiny render functions and observers.
#'
#' @param session Shiny session object.
#' @param cohort_id Id of the cohort.
#' @param step_id Id of the step to check. When NULL (default) all the steps are checked for changes.
#'
#' @export
cb_changed <- function(session, cohort_id, step_id = NULL) {
  ns <- session$ns
  if (!is.null(step_id)) {
    step_id <- paste0("_", step_id)
  }
  session$input[[ns(paste0(cohort_id, "-cb_data_updated", step_id))]]
}


#' JS snippet to relocate a modal dialog in the DOM
#'
#' Generates jQuery that re-parents a modal so it is not clipped by its
#' originating container (appended to the document body or to the cohort's
#' `.cb_inputs` container).
#'
#' @param dialog_id Id of the modal dialog element to move.
#' @param container_id Id of the cohort container (used when `where = "container"`).
#' @param where Either `"body"` (default), `"container"`, or a jQuery selector string.
#' @return A character string of JavaScript.
#' @noRd
move_modal_dialog_js <- function(dialog_id, container_id, where = "body") {
  if (where == "container") {
    return(paste0("$('#", dialog_id, "').appendTo('#", container_id, " .cb_inputs');"))
  }
  paste0("$('#", dialog_id, "').appendTo('", where, "');")
}
