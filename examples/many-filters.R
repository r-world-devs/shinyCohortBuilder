library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
#options("shiny.minified" = FALSE)
#options("shiny.trace" = TRUE)
options("cb_render_all" = FALSE)
options("cb_active_filter" = FALSE)
#
# simple_m <- function(message, call = NULL) {
#   message <- paste(Sys.time(), strtrim(message, 100), "\n")
#   structure(
#     list(message = message, call = call),
#     class = c("simpleMessage", "message", "condition")
#   )
# }
# rlang::env_unlock(env = asNamespace('base'))
# rlang::env_binding_unlock(env = asNamespace('base'))
# assign('simpleMessage', simple_m, envir = asNamespace('base'))
# rlang::env_binding_lock(env = asNamespace('base'))
# rlang::env_lock(asNamespace('base'))

set.seed(123)
n <- 2000
n_range <- 20
n_disc <- 30
n_date_range <- 30
v_range <- rnorm(n)
v_disc <- function() sample(paste0("a", 1:n), n, TRUE)
v_date_range <- sample(seq.Date(Sys.Date(), by = "day", length.out = 500), n, TRUE)

datasets <- tblist(
  range = purrr::map_dfc(1:n_range, ~v_range),
  disc = purrr::map_dfc(1:n_disc, ~v_disc()),
  date_range = purrr::map_dfc(1:n_date_range, ~v_date_range)
)
names(datasets$range) <- paste0("range_", 1:n_range)
names(datasets$disc) <- paste0("disc_", 1:n_disc)
names(datasets$date_range) <- paste0("date_range_", 1:n_date_range)

filters_range <- purrr::map(
  1:n_range,
  ~ filter("range", variable = paste0("range_", .x), dataset = "range")
)
filters_disc <- purrr::map(
  1:n_disc,
  ~ filter("discrete", variable = paste0("disc_", .x), dataset = "disc", gui_input = "vs")
)
filters_date_range <- purrr::map(
  1:n_date_range,
  ~ filter("date_range", variable = paste0("date_range_", .x), dataset = "date_range")
)
filters <- append(
  append(
    filters_range,
    filters_disc
  ),
  filters_date_range
)

ui <- fluidPage(
  cb_ui(id = "patients", style = "width: 300px; float: left;", steps = FALSE, state = FALSE),
  shiny::div(
    style = "float: right; width: calc(100% - 300px);",
    shiny::verbatimTextOutput("datasets")
  )
)

server <- function(input, output, session) {

  raw_source <- cohortBuilder::set_source(
    datasets
  )

  coh <- cohortBuilder::cohort(
    raw_source,
    do.call(
      cohortBuilder::step,
      filters
    )
  )

  cb_server(id = "patients", coh, run_button = TRUE, stats = c("pre", "post"), feedback = TRUE)

  returned_data <- shiny::eventReactive(input[["patients-cb_data_updated"]], {
    coh$get_data(step_id = coh$last_step_id(), state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  output$datasets <- shiny::renderPrint({
    print(returned_data())
  })

}

shinyApp(ui, server)
