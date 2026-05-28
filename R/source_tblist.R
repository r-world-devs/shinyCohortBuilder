get_filter_dataset <- function(filter) {
  environment(filter$filter_data)$dataset
}

group_filters <- function(source, filters) {
  datasets <- names(source$dtconn)
  data_filters <- purrr::map_chr(filters, get_filter_dataset)
  ordered_filters <- list()
  for (dataset in datasets) {
    ordered_filters <- append(
      ordered_filters,
      list(filters[data_filters == dataset])
    )
  }

  stats::setNames(ordered_filters, datasets)
}

dataset_help_icon <- function(cohort, dataset_name, ns) {
  if (!isTRUE(cohort$attributes$show_help)) return(NULL)
  if (is.null(cohort$show_help(field = dataset_name))) return(NULL)

  shiny::a(
    href = "#",
    class = "dataset_tooltip",
    getOption("scb_icons", scb_icons)$dataset_help_icon %>%
      shiny::tagAppendAttributes(
        onclick = .trigger_action_js("show_help", list(field = dataset_name), ns = ns)
      )
  )
}

dataset_filters <- function(filters, dataset_name, step_id, cohort, ns) {
  stats_id <- ns(paste0(step_id, "-stats_", dataset_name))
  no_filters_class <- ""
  if (length(filters) == 0) {
    no_filters_class <- "no-filters"
  }
  shiny::div(
    class = c("cb_filters_group", dataset_name, no_filters_class),
    shiny::tags$strong(dataset_name),
    dataset_help_icon(cohort, dataset_name, ns),
    shiny::htmlOutput(stats_id, inline = TRUE, style = "float: right; "),
    filters %>%
      purrr::map(
        ~ .render_filter(.x, step_id, cohort, ns = ns)
      )
  )
}

#' @rdname rendering-filters
#' @export
.render_filters.tblist <- function(source, cohort, step_id, ns, ...) {
  step <- cohort$get_step(step_id)

  group_filters(cohort$get_source(), step$filters) %>%
    purrr::imap(~ dataset_filters(.x, .y, step_id, cohort, ns = ns)) %>%
    shiny::div(class = "cb_filters", `data-step_id` = step_id)
}

#' @rdname updating-data-statistics
#' @export
.update_data_stats.tblist <- function(source, step_id, cohort, session, ...) {
  stats <- cohort$attributes$stats
  step <- cohort$get_step(step_id)

  dataset_names <- names(cohort$get_source()$dtconn)
  data_filters <- purrr::map_chr(step$filters, get_filter_dataset)
  dataset_names <- intersect(dataset_names, data_filters)

  dataset_names %>% purrr::walk(
    ~ .sendOutput(
      paste0(step_id, "-stats_", .x),
      shiny::renderUI({
        previous <- cohort$get_cache(step_id, state = "pre")[[.x]]$n_rows
        if (!previous > 0) {
          return("No data selected in previous step.")
        }
        current <- cohort$get_cache(step_id, state = "post")[[.x]]$n_rows
        .pre_post_stats(current, previous, percent = TRUE, stats = stats)
      }),
      session
    )
  )
}

#' @rdname rendering-step-attrition
#' @export
.step_attrition.tblist <- function(source, id, cohort, session, ...) {
  ns <- session$ns
  choices <- names(source$dtconn)

  list(
    render = shiny::renderPlot({
      cohort$show_attrition(dataset = session$input$attrition_input)
    }),
    output = shiny::tagList(
      shiny::selectInput(ns("attrition_input"), "Choose dataset", choices),
      shiny::plotOutput(id)
    )
  )
}

#' @rdname available-filters-choices
#' @export
.available_filters_choices.tblist <- function(source, cohort, ...) {

  available_filters <- cohort$attributes$available_filters

  choices <- purrr::map(available_filters, function(x) {
    tibble::tibble(
      name = as.character(
        shiny::div(
          `data-tooltip-z-index` = 9999,
          `data-tooltip` = x$get_params("description"),
          `data-tooltip-position` = "top right",
          `data-tooltip-allow-html` = "true",
          x$name
        )
      ),
      id = x$id,
      dataset = x$get_params("dataset")
    )
  }) %>% dplyr::bind_rows()
  choices$name <- gsub("\"", "'", choices$name) # prevents invalid interpolation for setting labels

  shinyWidgets::prepare_choices(choices, name, id, dataset)
}

#' @rdname filter-position
#' @export
.filter_position.tblist <- function(source, step_id, filter, ns, ...) {
  return(glue::glue('#{ns(step_id)} .{filter$get_params("dataset")}'))
}
