# Domain-aware rendering playground
#
# Interactive configurator for shinyCohortBuilder's domain-aware rendering and
# deferred-run (run button) behaviour. Instead of showing fixed configurations
# side by side, this app lets you assemble a configuration and apply it:
#
#   * Right sidebar  - choose stats / feedback / propagate_domains /
#                      render_source / run_button, then click "Apply".
#   * Left sidebar   - the filtering panel built for the applied configuration.
#                      Explore how filters render and how steps behave.
#   * Main content   - the filtered data produced by the cohort.
#
# Background:
#   propagate_domains (a cohortBuilder concern) decides how a step narrows the
#   domains of downstream steps:
#     "none"   - domains never narrow (user-declared domains are still honoured).
#     "filter" - narrowed from each filter's own post-filter values.
#     "cache"  - narrowed from the cached statistics of the previous step.
#     "data"   - narrowed from the actual data remaining after the previous step.
#   render_source (a shinyCohortBuilder concern) decides whether the UI renders
#   from those domains ("domain") or from cached statistics ("auto").
#   render_source = "domain" requires every filter to declare a domain; the demo
#   filters always do, so it works under any propagate_domains mode.
#
# Run with:
#   shiny::runApp(system.file("examples/domains", package = "shinyCohortBuilder"))

options("tryCatchLog.include.full.call.stack" = FALSE)

library(shiny)
library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)

iris$Species <- as.character(iris$Species)

# Filters declare a `domain` so they can be rendered without reading the data.
# `species_domain` / `sepal_domain` may be NULL to drop the declared domain for
# that filter (the filter then falls back to data-derived values).
build_filters <- function(species_domain = c("setosa", "versicolor", "virginica"),
                          sepal_domain = c(4, 8)) {
  list(
    filter(
      "discrete",
      id = "species", name = "Species",
      dataset = "iris", variable = "Species",
      # Full declared vocabulary - a superset of what any single step contains.
      domain = species_domain
    ),
    filter(
      "range",
      id = "sepal_length", name = "Sepal length",
      dataset = "iris", variable = "Sepal.Length",
      domain = sepal_domain
    )
  )
}

# The two flags are independent:
#   * `available_filters` attaches the filters to the source as available
#     filters (so the user can add them via the "manage step" UI).
#   * `add_initial` adds the filters as active step filters on initial build.
# With both FALSE the cohort starts empty and offers nothing to add.
build_cohort <- function(cache = TRUE, propagate_domains = "filter",
                         species_domain = c("setosa", "versicolor", "virginica"),
                         sepal_domain = c(4, 8),
                         available_filters = TRUE,
                         add_initial = TRUE) {
  filters <- build_filters(species_domain, sepal_domain)
  source <- if (available_filters) {
    set_source(tblist(iris = iris), available_filters = filters)
  } else {
    set_source(tblist(iris = iris))
  }
  coh <- cohort(
    source = source,
    cache = cache,
    propagate_domains = propagate_domains
  )
  if (add_initial) {
    coh <- coh %>%
      add_filter(filters[[1]]) %>%
      add_filter(filters[[2]])
  }
  coh
}

# Map the "stats" selectInput value to the cb_server() `stats` argument.
stats_arg <- function(value) {
  switch(value,
    "pre+post" = c("pre", "post"),
    "pre"      = "pre",
    "post"     = "post",
    "none"     = NULL
  )
}

config_sidebar <- function() {
  bslib::sidebar(
    title = "Configuration",
    position = "right",
    open = TRUE,
    width = 320,
    shiny::selectInput(
      "cfg_stats", "stats",
      choices = c("pre+post", "pre", "post", "none"),
      selected = "pre+post"
    ),
    shiny::checkboxInput("cfg_feedback", "feedback", value = TRUE),
    shiny::checkboxInput("cfg_cache", "cache", value = TRUE),
    shiny::selectInput(
      "cfg_propagate", "propagate_domains",
      choices = c("none", "filter", "cache", "data"),
      selected = "filter"
    ),
    shiny::selectInput(
      "cfg_render", "render_source",
      choices = c("auto", "domain"),
      selected = "auto"
    ),
    shiny::selectInput(
      "cfg_run_button", "run_button",
      choices = c("none", "local", "global"),
      selected = "none"
    ),
    shiny::selectInput(
      "cfg_species_domain", "Species domain",
      choices = c("setosa", "versicolor", "virginica"),
      selected = c("setosa", "versicolor", "virginica"),
      multiple = TRUE
    ),
    shiny::checkboxInput("cfg_sepal_domain_on", "Sepal length domain", value = TRUE),
    shiny::sliderInput(
      "cfg_sepal_domain", NULL,
      min = 0, max = 10, value = c(4, 8), step = 0.1
    ),
    shiny::checkboxInput("cfg_available_filters", "available_filters", value = TRUE),
    shiny::checkboxInput("cfg_add_initial", "filters on initial build", value = TRUE),
    shiny::actionButton(
      "cfg_apply", "Apply",
      class = "btn-primary", width = "100%"
    ),
    shiny::tags$small(
      class = "text-muted",
      "render_source = 'domain' requires every filter to declare a domain. ",
      "The demo filters always do."
    )
  )
}

ui <- bslib::page_sidebar(
  title = "Domain-aware rendering playground",
  sidebar = config_sidebar(),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filtering panel",
      open = TRUE,
      width = 340,
      shiny::uiOutput("filter_panel")
    ),
    bslib::navset_card_tab(
      title = "Cohort",
      bslib::nav_panel(
        "Filtered data",
        shiny::verbatimTextOutput("filtered_summary"),
        shiny::tableOutput("filtered_table")
      ),
      bslib::nav_panel(
        "Cohort cache",
        shiny::verbatimTextOutput("cohort_cache")
      )
    )
  )
)

server <- function(input, output, session) {

  # Each Apply mounts a fresh cb_server module under a unique id so the new
  # configuration starts from a clean state (re-using one id would keep the
  # previous module's reactive wiring).
  applied <- shiny::reactiveValues(id = NULL, cohort = NULL)
  apply_counter <- shiny::reactiveVal(0L)

  shiny::observeEvent(input$cfg_apply, {
    n <- apply_counter() + 1L
    apply_counter(n)
    module_id <- paste0("cohort_", n)

    # A filter with no selected / disabled domain is built with domain = NULL.
    species_domain <- input$cfg_species_domain
    if (length(species_domain) == 0) species_domain <- NULL
    sepal_domain <- if (isTRUE(input$cfg_sepal_domain_on)) input$cfg_sepal_domain else NULL

    # cache mainly affects stats; with it off, stats modes have no cached data.
    # propagate_domains = "cache" also requires cache = TRUE, so surface that
    # (and any other construction error) as a friendly notification.
    coh <- tryCatch(
      build_cohort(
        cache = isTRUE(input$cfg_cache),
        propagate_domains = input$cfg_propagate,
        species_domain = species_domain,
        sepal_domain = sepal_domain,
        available_filters = isTRUE(input$cfg_available_filters),
        add_initial = isTRUE(input$cfg_add_initial)
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(coh)) return(invisible(NULL))

    # render_source = "domain" needs every filter to have a domain. Validate up
    # front so a bad combination surfaces as a friendly notification instead of
    # erroring inside the module.
    if (identical(input$cfg_render, "domain")) {
      ok <- tryCatch(
        {
          shinyCohortBuilder:::validate_domains_present(coh)
          TRUE
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
          FALSE
        }
      )
      if (!ok) return(invisible(NULL))
    }

    applied$id <- module_id
    applied$cohort <- coh

    # Capture config values now; the onFlushed callback below runs outside a
    # reactive context and cannot read input$ values directly.
    cfg_run_button <- input$cfg_run_button
    cfg_stats <- stats_arg(input$cfg_stats)
    cfg_feedback <- isTRUE(input$cfg_feedback)
    cfg_render <- input$cfg_render

    # cb_server() renders steps by inserting UI into the cb_ui() accordion. That
    # accordion is produced by output$filter_panel (a renderUI reacting to
    # applied$id) which only flushes after this observer completes. Defer
    # cb_server() until the panel is in the DOM, otherwise the "Step 1" insert
    # targets an element that does not exist yet and is lost.
    session$onFlushed(function() {
      cb_server(
        module_id,
        coh,
        run_button = cfg_run_button,
        stats = cfg_stats,
        feedback = cfg_feedback,
        render_source = cfg_render
      )
    }, once = TRUE)
  })

  output$filter_panel <- shiny::renderUI({
    if (is.null(applied$id)) {
      return(shiny::div(
        class = "text-muted",
        "Configure options on the right and click Apply to build the panel."
      ))
    }
    cb_ui(
      applied$id,
      steps = TRUE, state = FALSE, code = TRUE, attrition = TRUE,
      manage_step = TRUE
    )
  })

  # The filtered data is recomputed whenever the active cohort signals an update.
  filtered_data <- shiny::reactive({
    shiny::req(applied$id, applied$cohort)
    # React to the active module's data-updated signal.
    input[[paste0(applied$id, "-cb_data_updated")]]
    applied$cohort$get_data(state = "post")
  })

  output$filtered_summary <- shiny::renderPrint({
    data <- filtered_data()
    tbl <- data$iris
    if (is.null(tbl)) {
      cat("No data yet - run the step to compute the filtered result.\n")
      return(invisible(NULL))
    }
    cat("Rows:", nrow(tbl), "of", nrow(iris), "\n")
    cat("Species:", paste(sort(unique(tbl$Species)), collapse = ", "), "\n")
    cat(
      "Sepal.Length range:",
      if (nrow(tbl)) paste(range(tbl$Sepal.Length), collapse = " - ") else "-",
      "\n"
    )
  })

  output$filtered_table <- shiny::renderTable({
    data <- filtered_data()
    shiny::req(data$iris)
    utils::head(data$iris, 20)
  })

  # Print the cohort's internal cache so you can watch how stats are stored per
  # step (cache id "0" is the source baseline; "1", "2", ... are the steps) and
  # how propagate_domains / cache settings change what is computed.
  #
  # The cache is also populated lazily (stats computed during render, run-button
  # pending state, on-demand get_cache reads) outside the cb_data_updated signal,
  # so polling on a short timer keeps the view live and catches those updates.
  output$cohort_cache <- shiny::renderPrint({
    shiny::req(applied$id, applied$cohort)
    # Re-render on the data-updated signal AND every second so lazily populated
    # cache entries show up without needing another explicit trigger.
    input[[paste0(applied$id, "-cb_data_updated")]]
    shiny::invalidateLater(1000, session)
    cache <- applied$cohort$.__enclos_env__$private$cache
    if (length(cache) == 0) {
      cat("Cache is empty.\n")
      return(invisible(NULL))
    }
    utils::str(cache, max.level = 4)
  })
}

shiny::shinyApp(ui, server, options = list(port = 8888))
