get_filter_dataset <- function(filter) {
  environment(filter$filter_data)$dataset
}

group_filters <- function(source, filters) {
  datasets <- names(source$dtconn)
  data_filters <- purrr::map_chr(filters, get_filter_dataset)
  datasets <- intersect(datasets, data_filters)
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
    shiny::icon(
      "question-circle",
      onclick = .trigger_action_js("show_help", list(field = dataset_name), ns = ns)
    )
  )
}

dataset_filters <- function(filters, dataset_name, step_id, cohort, ns) {
  stats_id <- ns(paste0(step_id, "-stats_", dataset_name))
  shiny::tagList(
    shiny::tags$strong(dataset_name),
    dataset_help_icon(cohort, dataset_name, ns),
    shiny::htmlOutput(stats_id, inline = TRUE, style = "float: right;"),
    shiny::hr(style = "margin-top: 0.3rem;"),
    filters %>%
      purrr::map(
        ~ .render_filter(.x, step_id, cohort, ns = ns)
      ),
    shiny::div(style = "padding-top: 1rem; padding-bottom: 1rem;")
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

drop_nulls <- function(x) {
  purrr::keep(x, ~!is.null(.))
}

rule_character <- function(column, name, dataset_name) {
  type <- "discrete"
  gui_input <- NULL
  n_unique <- length(unique(column))
  if (n_unique == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  drop_nulls(
    list(
      type = type,
      #id = name,
      name = name,
      variable = name,
      dataset = dataset_name,
      value = NA,
      keep_na = TRUE,
      gui_input = gui_input
    )
  )
}

rule_factor <- function(column, name, dataset_name) {
  type <- "discrete"
  gui_input <- NULL
  n_levels <- length(levels(column))
  if (n_levels == length(column)) {
    type <- "discrete_text"
  } else if (length(unique(column)) > 3) {
    gui_input <- "vs"
  }
  drop_nulls(
    list(
      type = type,
      #id = name,
      name = name,
      variable = name,
      dataset = dataset_name,
      value = NA,
      keep_na = TRUE,
      gui_input = gui_input
    )
  )
}

rule_numeric <- function(column, name, dataset_name) {
  list(
    type = "range",
    #id = name,
    name = name,
    variable = name,
    dataset = dataset_name,
    range = NA,
    keep_na = TRUE
  )
}
rule_integer <- rule_numeric

rule_Date <- function(column, name, dataset_name) {
  list(
    type = "date_range",
    #id = name,
    name = name,
    variable = name,
    dataset = dataset_name,
    range = NA,
    keep_na = TRUE
  )
}

filter_rule <- function(column, name, dataset_name) {
  rule_method <- paste0("rule_", class(column))
  do.call(
    rule_method,
    list(
      column = column,
      name = name,
      dataset_name = dataset_name
    )
  )
}

filter_rules <- function(dataset, dataset_name) {
  dataset %>%
    purrr::imap(~filter_rule(.x, .y, dataset_name = dataset_name))
}

#' @rdname autofilter
#' @export
autofilter.tblist <- function(source, attach_as = c("step", "meta"), ...) {
  attach_as <- rlang::arg_match(attach_as)
  step_rule <- source$dtconn %>%
    purrr::imap(~filter_rules(.x, .y)) %>%
    unlist(recursive = FALSE) %>%
    purrr::map(~do.call(cohortBuilder::filter, .)) %>%
    unname()

  if (identical(attach_as, "meta")) {
    source$attributes$available_filters <- step_rule
  } else {
    source %>%
      cohortBuilder::add_step(do.call(cohortBuilder::step, step_rule))
  }

  return(source)
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

  shinyWidgets::prepare_choices(choices, name, id, dataset)
}
