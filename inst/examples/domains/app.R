# Domain-aware rendering demo
#
# Demonstrates how shinyCohortBuilder renders filter inputs from a filter's
# declared `domain` instead of from cached statistics, and how the
# `render_source` argument and the `stats` / `feedback` / `cache` settings
# interact.
#
# Three configurations are shown side by side:
#   1. Stats mode  - stats + feedback on (current behaviour). The `cache` flag
#                    only changes *when* statistics are computed (eager vs lazy).
#   2. Domain mode - stats = NULL, feedback = FALSE, cache = FALSE. Filters render
#                    purely from their domain; the source is never scanned.
#   3. Domain bounds in stats mode - render_source = "domain" with stats on:
#                    full domain vocabulary/bounds with counts overlaid.
#
# Run with:
#   shiny::runApp(system.file("examples/domains", package = "shinyCohortBuilder"))

library(shiny)
library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)

iris$Species <- as.character(iris$Species)

# Filters declare a `domain` so they can be rendered without reading the data.
build_cohort <- function(cache = TRUE) {
  cohort(
    source = set_source(tblist(iris = iris)),
    cache = cache
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

panel <- function(title, id) {
  shiny::column(
    width = 4,
    shiny::h4(title),
    cb_ui(id, steps = TRUE, state = FALSE, code = TRUE, attrition = TRUE)
  )
}

ui <- shiny::fluidPage(
  shiny::titlePanel("Domain-aware filter rendering"),
  shiny::fluidRow(
    panel("1. Stats mode (cache = TRUE)", "stats_mode"),
    panel("2. Domain mode (stats = NULL)", "domain_mode"),
    panel("3. render_source = 'domain'", "domain_bounds")
  )
)

server <- function(input, output, session) {
  # 1. Classic stats mode: choices/ranges and counts come from the cache.
  cb_server(
    "stats_mode",
    build_cohort(cache = TRUE),
    stats = c("pre", "post"),
    feedback = TRUE,
    render_source = "auto"
  )

  # 2. Pure domain mode: stats disabled, so inputs render from the domain and the
  #    source is never scanned (no plots, no data stats).
  cb_server(
    "domain_mode",
    build_cohort(cache = FALSE),
    stats = NULL,
    feedback = FALSE,
    render_source = "auto"
  )

  # 3. Stats on, but choice/range bounds come from the full domain with counts
  #    overlaid where available.
  cb_server(
    "domain_bounds",
    build_cohort(cache = TRUE),
    stats = c("pre", "post"),
    feedback = TRUE,
    render_source = "domain"
  )
}

shiny::shinyApp(ui, server)
