library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
options("cb_active_filter" = FALSE)

datasets = tblist(
  patients = data.frame(id = letters[1:10], age = 20:29),
  deaths = data.frame(
    patient_id = letters[1:6],
    type_id = c("sudden", "sudden", "sudden", "prolonged", "prolonged", "prolonged"),
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 6)
  ),
  death_types = data.frame(id = c("sudden", "prolonged"), label = c("Sudden", "Prolonged"))
)

age_filter <- filter(type = "range", id = "age", range = NA, variable = "age", dataset = "patients", name = "Age", active = FALSE)
death_date_filter <- filter(type = "date_range", id = "death_date", range = NA, variable = "date", dataset = "deaths", name = "Date of Death", active = FALSE)
death_type_filter <- filter(type = "discrete", id = "death_type", value = NA, variable = "label", dataset = "death_types", name = "Death Type", active = FALSE)

binding_keys <- cohortBuilder::bind_keys(
  cohortBuilder::bind_key(
    update = cohortBuilder::data_key("deaths", "type_id"),
    cohortBuilder::data_key("death_types", "id"),
    post = TRUE
  ),
  cohortBuilder::bind_key(
    update = cohortBuilder::data_key("patients", "id"),
    cohortBuilder::data_key("deaths", "patient_id"),
    post = TRUE
  ),
  cohortBuilder::bind_key(
    update = cohortBuilder::data_key("deaths", "patient_id"),
    cohortBuilder::data_key("patients", "id"),
    post = TRUE
  )
)

ui <- fluidPage(
  cb_ui(id = "patients", style = "width: 300px; float: left;", steps = TRUE, state = FALSE),
  shiny::div(
    style = "float: right; width: calc(100% - 300px);",
    shiny::verbatimTextOutput("datasets")
  )
)

server <- function(input, output, session) {

  raw_source <- cohortBuilder::set_source(
    datasets,
    binding_keys = binding_keys,
    primary_keys = cohortBuilder::primary_keys(
      cohortBuilder::data_key("patients", "id"),
      cohortBuilder::data_key("deaths", "patient_id"),
      cohortBuilder::data_key("death_types", "id")
    ),
    source_code = substitute({
      source <- list(attributes = list(datasets = datasets_value))
    },
    list(
      datasets_value = dput(datasets, file = tempfile())
    ))
  )

  coh <- cohortBuilder::cohort(
    raw_source,
    cohortBuilder::step(
      age_filter,
      death_date_filter,
      death_type_filter
    )
  )

  cb_server(id = "patients", coh, run_button = TRUE, stats = c("pre", "post"), feedback = FALSE)

  returned_data <- shiny::eventReactive(input[["patients-cb_data_updated"]], {
    coh$get_data(step_id = coh$last_step_id(), state = "post")
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  output$datasets <- shiny::renderPrint({
    print(returned_data())
  })

}

shinyApp(ui, server)
