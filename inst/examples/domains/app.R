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

library(shiny)
library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)

iris$Species <- as.character(iris$Species)

# Filters declare a `domain` so they can be rendered without reading the data.
build_cohort <- function(cache = TRUE, propagate_domains = "filter") {
  cohort(
    source = set_source(tblist(iris = iris)),
    cache = cache,
    propagate_domains = propagate_domains
  ) %>%
    add_filter(
      filter(
        "discrete",
        id = "species", name = "Species",
        dataset = "iris", variable = "Species",
        # Full declared vocabulary - a superset of what any single step contains.
        domain = c("setosa", "versicolor", "virginica")
      )
    ) %>%
    add_filter(
      filter(
        "range",
        id = "sepal_length", name = "Sepal length",
        dataset = "iris", variable = "Sepal.Length",
        domain = c(4, 8)
      )
    )
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
    bslib::card(
      bslib::card_header("Filtered data"),
      shiny::verbatimTextOutput("filtered_summary"),
      shiny::tableOutput("filtered_table")
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

    # cache only matters for stats; keep it on so stats modes have data to show.
    coh <- build_cohort(cache = TRUE, propagate_domains = input$cfg_propagate)

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

    cb_server(
      module_id,
      coh,
      run_button = input$cfg_run_button,
      stats = stats_arg(input$cfg_stats),
      feedback = isTRUE(input$cfg_feedback),
      render_source = input$cfg_render
    )
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
      steps = TRUE, state = FALSE, code = TRUE, attrition = TRUE
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
    utils::head(data$iris, 20)
  })
}

shiny::shinyApp(ui, server, options = list(port = 8888))
