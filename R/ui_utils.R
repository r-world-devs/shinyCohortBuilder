button <- function(..., icon = NULL, type = "btn-outline-dark btn-sm") {
  shiny::tags$button(type = "button", class = paste("scb_button btn btn-default", type), icon, ...)
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

