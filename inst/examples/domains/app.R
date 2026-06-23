# Domain-aware rendering demo
#
# Demonstrates how shinyCohortBuilder renders filter inputs from a filter's
# declared `domain` instead of from cached statistics, how the `render_source`
# argument interacts with the `stats` / `feedback` / `cache` settings, and how
# domain propagation behaves together with the `run_button` (deferred run) mode.
#
# The cohort's `propagate_domains` mode (a cohortBuilder concern) decides how a
# step narrows the domains of downstream steps:
#   "none"   - domains never narrow.
#   "filter" - narrowed from each filter's own post-filter values.
#   "cache"  - narrowed from the cached statistics of the previous step.
#   "data"   - narrowed from the actual data remaining after the previous step.
# shinyCohortBuilder's `render_source` only decides whether the *UI* renders
# from those domains ("domain") or from cached stats ("auto").
#
# Six configurations are shown, three per row:
#
# Row 1 - immediate run (run_button = "none"):
#   1. Stats mode       - render_source = "auto", stats + feedback on. Choices,
#                         ranges and counts come from the cache. `cache` only
#                         changes *when* statistics are computed (eager vs lazy).
#   2. Domain mode      - render_source = "auto", stats = NULL, feedback = FALSE,
#                         cache = FALSE. Inputs render from the domain and the
#                         source is never scanned (no plots, no data stats).
#   3. Domain-driven UI - render_source = "domain", stats = NULL. Choice/range
#                         bounds come from the propagated domain; downstream
#                         steps re-render as upstream filters narrow them.
#
# Row 2 - deferred run (run button on): step computations run only when the run
# button is clicked, so domains propagate on click rather than on every edit.
#   4. Stats + global run    - render_source = "auto", stats on, run_button =
#                              "global". One run button in the top panel applies
#                              all pending steps at once.
#   5. Domain + local run    - render_source = "domain", stats = NULL,
#                              run_button = "local". Each step panel has its own
#                              run button; domains propagate downstream on click.
#   6. Domain + stats global - render_source = "domain", stats on, run_button =
#                              "global". Domain-driven choices with counts
#                              overlaid, recomputed when global run is clicked.
#
# Run with:
#   shiny::runApp(system.file("examples/domains", package = "shinyCohortBuilder"))

library(shiny)
library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)

iris$Species <- as.character(iris$Species)

# Filters declare a `domain` so they can be rendered without reading the data.
# `propagate_domains` controls how downstream step domains are narrowed.
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

panel <- function(title, id) {
  shiny::column(
    width = 4,
    shiny::h4(title),
    cb_ui(id, steps = TRUE, state = FALSE, code = TRUE, attrition = TRUE)
  )
}

ui <- shiny::fluidPage(
  shiny::titlePanel("Domain-aware filter rendering"),
  shiny::h3("Immediate run (run_button = \"none\")"),
  shiny::fluidRow(
    panel("1. Stats mode (render_source = 'auto')", "stats_mode"),
    panel("2. Domain mode (stats = NULL)", "domain_mode"),
    panel("3. Domain-driven UI (render_source = 'domain')", "domain_bounds")
  ),
  shiny::hr(),
  shiny::h3("Deferred run (run button on)"),
  shiny::fluidRow(
    panel("4. Stats + global run button", "stats_global"),
    panel("5. Domain + local run button", "domain_local"),
    panel("6. Domain + stats + global run", "domain_global")
  )
)

server <- function(input, output, session) {

  ## Row 1 - immediate run -----------------------------------------------------

  # 1. Classic stats mode: choices/ranges and counts come from the cache.
  cb_server(
    "stats_mode",
    build_cohort(cache = TRUE, propagate_domains = "cache"),
    stats = c("pre", "post"),
    feedback = TRUE,
    render_source = "auto"
  )

  # 2. Pure domain mode: stats disabled, so inputs render from the domain and the
  #    source is never scanned (no plots, no data stats).
  cb_server(
    "domain_mode",
    build_cohort(cache = FALSE, propagate_domains = "data"),
    stats = NULL,
    feedback = FALSE,
    render_source = "auto"
  )

  # 3. Domain-driven UI: choice/range bounds come from the propagated domain;
  #    downstream steps re-render as upstream filters narrow their domains.
  cb_server(
    "domain_bounds",
    build_cohort(cache = TRUE, propagate_domains = "filter"),
    stats = NULL,
    feedback = FALSE,
    render_source = "domain"
  )

  ## Row 2 - deferred run (run button on) --------------------------------------

  # 4. Stats mode with a single global run button: edits mark steps pending and
  #    propagation/stats are recomputed only when the button is clicked.
  cb_server(
    "stats_global",
    build_cohort(cache = TRUE, propagate_domains = "cache"),
    run_button = "global",
    stats = c("pre", "post"),
    feedback = TRUE,
    render_source = "auto"
  )

  # 5. Domain-driven UI with per-step (local) run buttons: each step runs on its
  #    own button, propagating its narrowed domain to the next step on click.
  cb_server(
    "domain_local",
    build_cohort(cache = TRUE, propagate_domains = "filter"),
    run_button = "local",
    stats = NULL,
    feedback = FALSE,
    render_source = "domain"
  )

  # 6. Domain-driven choices with stats counts overlaid, deferred behind a global
  #    run button: domains and counts are recomputed together on click.
  cb_server(
    "domain_global",
    build_cohort(cache = TRUE, propagate_domains = "cache"),
    run_button = "global",
    stats = c("pre", "post"),
    feedback = FALSE,
    render_source = "domain"
  )
}

shiny::shinyApp(ui, server, options = list(port = 8888))
