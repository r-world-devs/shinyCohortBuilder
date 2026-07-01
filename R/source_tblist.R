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

#' Extract plain description text from a `describe()` object or string
#'
#' Mirrors cohortBuilder's internal `description_text()`: pulls `$text` from a
#' `describe()` list, accepts a bare non-empty string, and returns `NA` for
#' anything empty/missing.
#'
#' @param x A `describe()` object, character string, or `NULL`.
#' @return A single description string, or `NA_character_`.
#' @noRd
.description_text <- function(x) {
  if (is.list(x)) x <- x$text
  if (is.null(x) || !is.character(x) || !nzchar(x)) return(NA_character_)
  x
}

#' Build the virtual-select choice description for an available filter
#'
#' Combines the filter-level description (from the filter's own `@description`)
#' with the per-variable descriptions taken from `shape()`. The filter-level
#' text (when present) comes first, followed by the variable descriptions:
#' single-variable filters show the bare variable description, while
#' multi-variable filters prefix each with its variable name. Empty parts are
#' dropped, so a filter with only variable descriptions (the autofilter case)
#' shows just those.
#'
#' @param filter An S7 filter object.
#' @param shape_entry The filter's entry from `shape()$filters` (provides
#'   `variables`), or `NULL`.
#' @return A single description string, or `NA_character_` when nothing usable.
#' @noRd
.choice_description <- function(filter, shape_entry) {
  filter_part <- .description_text(filter@description)

  variables <- shape_entry$variables %||% list()
  multi <- length(variables) > 1
  var_parts <- purrr::map_chr(variables, function(v) {
    desc <- .description_text(v$description)
    if (is.na(desc)) return(NA_character_)
    if (multi) paste0(v$name, ": ", desc) else desc
  })
  var_part <- var_parts[!is.na(var_parts)]
  var_part <- if (length(var_part)) paste(var_part, collapse = "; ") else NA_character_

  parts <- c(filter_part, var_part)
  parts <- parts[!is.na(parts)]
  if (!length(parts)) return(NA_character_)
  paste(parts, collapse = " \u2014 ")
}

#' @rdname available-filters-choices
#' @export
.available_filters_choices.tblist <- function(source, cohort, ...) {

  available_filters <- cohort$attributes$available_filters

  # Pull per-filter (and per-variable) descriptions from `shape()`. Domains are
  # not needed here (we only render name + description), so skip computing them.
  filters_shape <- cohortBuilder::shape(source, domains = FALSE)$filters

  choices <- purrr::map(available_filters, function(x) {
    tibble::tibble(
      name = x@name,
      id = x@id,
      dataset = x@dataset,
      description = .choice_description(x, filters_shape[[x@id]])
    )
  }) |> dplyr::bind_rows()

  shinyWidgets::prepare_choices(choices, name, id, dataset, description = description)
}

#' @rdname filter-position
#' @export
.filter_position.tblist <- function(source, step_id, filter, ns, ...) {
  return(glue::glue('#{ns(step_id)} .{filter@dataset}'))
}
