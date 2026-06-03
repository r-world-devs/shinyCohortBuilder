library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)

source <- set_source(
  tblist(
    patients = data.frame(
      id = 1:10,
      group = factor(c("A", "B", "C", "B", "B", "C", "A", "B", "C", "B")),
      gender = factor(c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M")),
      age = c(50L, 44L, 38L, 49L, 45L, 33L, 43L, 35L, 40L, NA)
    )
  )
)

coh <- cohort(
  source,
  filter(
    "discrete", id = "gender", name = "Gender", dataset = "patients",
    variable = "gender", value = "M"
  ),
  filter(
    "range", id = "age", name = "Age", dataset = "patients",
    variable = "age", range = c(30, 50)
  )
)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  cb_ui("coh", style = "width: 350px; float: left;"),
  div(
    style = "float: right; width: calc(100% - 360px);",
    verbatimTextOutput("datasets")
  )
)

server <- function(input, output, session) {
  cb_server(
    "coh", coh,
    run_button = "none", stats = "post",
    feedback = FALSE, enable_bookmarking = "disable", show_help = FALSE
  )
  returned_data <- eventReactive(input[["coh-cb_data_updated"]], {
    coh$get_data(state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  output$datasets <- renderPrint(print(returned_data()))
}

shinyApp(ui, server)
