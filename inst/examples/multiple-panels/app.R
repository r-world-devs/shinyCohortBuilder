library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
options("cb_active_filter" = FALSE)

ui <- bslib::page_sidebar(
  sidebar = bslib::sidebar(
    cb_ui(id = "patients_left", style = "width: 250px;", steps = TRUE, state = FALSE),
    width = 300
  ),
  bslib::layout_sidebar(
    bslib::layout_columns(
      shiny::verbatimTextOutput("datasets_left"),
      shiny::verbatimTextOutput("datasets_right"),
      col_widths = c(0.5, 0.5)
    ),
    sidebar = bslib::sidebar(
      cb_ui(id = "patients_right", style = "width: 250px;", steps = TRUE, state = FALSE),
      position = "right", width = 300
    )
  )
)

server <- function(input, output, session) {

  raw_source_left <- cohortBuilder::set_source(as.tblist(librarian))
  coh_left <- cohortBuilder::cohort(
    raw_source_left,
    cohortBuilder::step(
      filter("discrete", id = "program", dataset = "borrowers", variable = "program"),
      filter("discrete", id = "name", dataset = "borrowers", variable = "name", gui_input = "vs"),
      filter("date_range", id = "registered", dataset = "borrowers", variable = "registered")
    )
  )

  raw_source_right <- cohortBuilder::set_source(as.tblist(librarian))
  coh_right <- cohortBuilder::cohort(
    raw_source_right,
    cohortBuilder::step(
      filter("discrete", id = "program", dataset = "borrowers", variable = "program"),
      filter("discrete", id = "name", dataset = "borrowers", variable = "name", gui_input = "vs"),
      filter("discrete_text", id = "id", dataset = "borrowers", variable = "id")
    )
  )

  cb_server(id = "patients_left", coh_left, stats = c("pre", "post"), feedback = FALSE)
  cb_server(id = "patients_right", coh_right, stats = c("pre"), feedback = TRUE)

  returned_data_left <- shiny::eventReactive(input[["patients_left-cb_data_updated"]], {
    coh_left$get_data(step_id = coh_left$last_step_id(), state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  returned_data_right <- shiny::eventReactive(input[["patients_right-cb_data_updated"]], {
    coh_right$get_data(step_id = coh_right$last_step_id(), state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  output$datasets_left <- shiny::renderPrint({
    print(returned_data_left())
  })
  output$datasets_right <- shiny::renderPrint({
    print(returned_data_right())
  })

}

shinyApp(ui, server)
