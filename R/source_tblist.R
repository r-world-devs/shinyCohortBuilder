#' Get the dataset a filter belongs to
#' @param filter A cohortBuilder filter object.
#' @return The filter's `@dataset` name.
#' @noRd
get_filter_dataset <- function(filter) {
  filter@dataset
}

#' Group filters by their source dataset
#'
#' Splits a list of filters into a named list keyed by dataset, preserving the
#' dataset ordering from the source connection.
#'
#' @param source A `tblist` source (provides `$dtconn` dataset names).
#' @param filters List of filter objects to group.
#' @return A named list of filter lists, one per dataset.
#' @noRd
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

#' Render a dataset's help tooltip icon
#'
#' Returns the clickable help icon for a dataset group, or `NULL` when help is
#' disabled or the dataset has no description.
#'
#' @param cohort The cohort (its `attributes$show_help` gates display).
#' @param dataset_name Name of the dataset.
#' @param ns Module namespace function.
#' @return An `<a>` `shiny.tag`, or `NULL`.
#' @noRd
dataset_help_icon <- function(cohort, dataset_name, ns) {
  if (!isTRUE(cohort$attributes$show_help)) return(NULL)
  if (is.null(cohort$show_help(field = dataset_name))) return(NULL)

  shiny::a(
    href = "#",
    class = "dataset_tooltip",
    getOption("scb_icons", scb_icons)$dataset_help_icon |>
      shiny::tagAppendAttributes(
        onclick = .trigger_action_js("show_help", list(field = dataset_name), ns = ns)
      )
  )
}

#' Render the filters group for a single dataset
#'
#' Builds the container holding a dataset's name, help icon, stats placeholder
#' and rendered filter inputs.
#'
#' @param filters List of the dataset's filters.
#' @param dataset_name Name of the dataset.
#' @param step_id Id of the step being rendered.
#' @param cohort The cohort object.
#' @param ns Module namespace function.
#' @return A `<div>` `shiny.tag` for the dataset group.
#' @noRd
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
    shiny::span(id = stats_id, style = "float: right; "),
    filters |>
      purrr::map(
        ~ .render_filter(.x, step_id, cohort, ns = ns)
      )
  )
}

#' @rdname rendering-filters
#' @export
.render_filters.tblist <- function(source, cohort, step_id, ns, ...) {
  step <- cohort$get_step(step_id)

  group_filters(cohort$get_source(), step$filters) |>
    purrr::imap(~ dataset_filters(.x, .y, step_id, cohort, ns = ns)) |>
    shiny::div(class = "cb_filters", `data-step_id` = step_id)
}

#' @rdname updating-data-statistics
#' @export
.update_data_stats.tblist <- function(source, step_id, cohort, session, ...) {
  stats <- cohort$attributes$stats
  # Data statistics follow the `stats` setting. When stats are disabled the
  # stats are not read, so this is a no-op (avoids forcing a source scan).
  if (is.null(stats)) {
    return(invisible(NULL))
  }
  step <- cohort$get_step(step_id)
  ns <- session$ns

  dataset_names <- names(cohort$get_source()$dtconn)
  data_filters <- purrr::map_chr(step$filters, get_filter_dataset)
  dataset_names <- intersect(dataset_names, data_filters)

  dataset_names |> purrr::walk(function(dataset) {
    selector <- paste0("#", ns(paste0(step_id, "-stats_", dataset)))
    # Read the parent (pre) snapshot stats, recomputing on demand when missing.
    # Every step's "pre" data is its parent's "post" snapshot, which always
    # exists: step 1's parent is the source, and steps 2+ are seeded from their
    # parent at construction / add_step (see Cohort$init_source / add_step). So
    # recomputing "pre" stats is safe in every mode (run_button, compute_stats = FALSE,
    # freshly added step) and lets step 1 show real stats before any run instead
    # of the "no data" placeholder. The placeholder is then reserved for its true
    # meaning: the parent step actually filtered out every row (previous == 0).
    # NULL is still handled defensively.
    pre_stats <- cohort$get_stats(step_id, state = "pre", .recalc_when_missing = TRUE)
    previous <- pre_stats[[dataset]]$n_rows
    if (is.null(previous) || !isTRUE(previous > 0)) {
      # Wrap in an element so the removeUI(" > *") cleanup below can remove it on
      # the next update. A bare string is inserted as a text node, which the
      # child-element selector cannot match, leaving stale text behind (e.g. the
      # placeholder lingering next to freshly computed stats after a run).
      ui <- shiny::tags$span("No data selected in previous step.")
    } else {
      current <- cohort$get_stats(step_id, state = "post")[[dataset]]$n_rows
      ui <- .pre_post_stats(current, previous, percent = TRUE, stats = stats)
    }
    shiny::removeUI(selector = paste0(selector, " > *"), multiple = TRUE, immediate = TRUE)
    shiny::insertUI(selector = selector, ui = ui, immediate = TRUE)
  })
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
          `data-tooltip` = x@description,
          `data-tooltip-position` = "top right",
          `data-tooltip-allow-html` = "true",
          x@name
        )
      ),
      id = x@id,
      dataset = x@dataset
    )
  }) |> dplyr::bind_rows()
  choices$name <- gsub("\"", "'", choices$name) # prevents invalid interpolation for setting labels

  shinyWidgets::prepare_choices(choices, name, id, dataset)
}

#' @rdname filter-position
#' @export
.filter_position.tblist <- function(source, step_id, filter, ns, ...) {
  return(glue::glue('#{ns(step_id)} .{filter@dataset}'))
}
