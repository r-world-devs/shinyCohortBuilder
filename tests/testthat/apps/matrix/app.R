# Parametrized app for UI matrix tests.
#
# All configuration is read from environment variables so a single app
# definition can cover the full cross-product of:
#   steps / feedback / run_button / propagate_domains / render_source / cache
# without maintaining one app directory per combination. The test harness sets
# these env vars (inherited by the spawned app process) before AppDriver$new().
#
# Recognised env vars (all optional, with defaults):
#   SCB_RUN_BUTTON        "none" | "local" | "global"        (default "none")
#   SCB_PROPAGATE         "none" | "filter" | "cache" | "data" (default "filter")
#   SCB_RENDER_SOURCE     "auto" | "domain"                  (default "auto")
#   SCB_CACHE             "TRUE" | "FALSE"                   (default "TRUE")
#   SCB_STATS             "pre+post" | "pre" | "post" | "none" (default "pre+post")
#   SCB_FEEDBACK          "TRUE" | "FALSE"                   (default "FALSE")

library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)

options("scb_minified" = FALSE)

env_chr <- function(name, default) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) default else val
}
env_lgl <- function(name, default) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) return(default)
  isTRUE(as.logical(val))
}

run_button     <- env_chr("SCB_RUN_BUTTON", "none")
propagate      <- env_chr("SCB_PROPAGATE", "filter")
render_source  <- env_chr("SCB_RENDER_SOURCE", "auto")
cache          <- env_lgl("SCB_CACHE", TRUE)
feedback       <- env_lgl("SCB_FEEDBACK", FALSE)
stats_raw      <- env_chr("SCB_STATS", "pre+post")
stats <- switch(stats_raw,
  "pre+post" = c("pre", "post"),
  "pre"      = "pre",
  "post"     = "post",
  "none"     = NULL,
  c("pre", "post")
)

# Data with a clear upstream/downstream domain-narrowing story: group "A" only
# ever occurs together with gender "M". Filtering gender to "F" in step 1
# therefore removes every "A" row, so a copied downstream step (step 2) should
# see its group domain narrowed to {B, C} when data/cache propagation is on, and
# its age range narrowed to the F-rows' range.
#
#   gender F rows: groups B/C only, ages 35-50
#   gender M rows: groups A/B/C,   ages 28-61 (and one NA)
patients <- data.frame(
  id = 1:12,
  group  = factor(c("B", "A", "C", "B", "C", "A", "A", "B", "C", "B", "A", "C")),
  gender = factor(c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M", "M", "F")),
  age    = c(50L, 28L, 38L, 49L, 45L, 33L, 43L, 35L, 40L, 61L, NA, 47L)
)

source <- set_source(tblist(patients = patients))

coh <- cohort(
  source,
  filter(
    "discrete", id = "gender", name = "Gender", dataset = "patients",
    variable = "gender", value = "F",
    # Declared domain so render_source = "domain" has something to render.
    domain = c("F", "M")
  ),
  filter(
    "range", id = "age", name = "Age", dataset = "patients",
    variable = "age", range = NA,
    domain = c(18, 80)
  ),
  filter(
    "discrete", id = "group", name = "Group", dataset = "patients",
    variable = "group", value = NA,
    domain = c("A", "B", "C")
  ),
  cache = cache,
  propagate_domains = propagate
)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  cb_ui(
    "coh", style = "width: 350px; float: left;",
    state = TRUE, code = TRUE, attrition = TRUE
  ),
  div(
    style = "float: right; width: calc(100% - 360px);",
    verbatimTextOutput("datasets")
  )
)

server <- function(input, output, session) {
  cb_server(
    "coh", coh,
    run_button = run_button,
    stats = stats,
    feedback = feedback,
    render_source = render_source,
    enable_bookmarking = "disable",
    show_help = FALSE
  )
  returned_data <- eventReactive(input[["coh-cb_data_updated"]], {
    coh$get_data(state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  output$datasets <- renderPrint(print(returned_data()))
}

shinyApp(ui, server)
