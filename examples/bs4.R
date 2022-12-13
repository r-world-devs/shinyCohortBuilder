library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)
library(bs4Dash)
options("cb_active_filter" = FALSE)

raw_source <- set_source(
  tblist(
    patients = data.frame(
      id = 1:10,
      group = c("A", "B", "C", "B", "B", "C", "A", "B", "C", "B"),
      gender = c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M"),
      age = c(sample(30:50, 9), NA),
      visit = sample(seq.Date(as.Date("1989-01-01"), as.Date("1991-01-01"), by = "month"), 10),
      biom1 = c("A", "B", "A", "A", "B", "B", "B", "A", "A", "A"),
      biom2 = c("C", "D", "C", "D", "E", "E", "C", "C", "E", "C")
    ),
    therapy = data.frame(
      id = 1:10,
      treatment = c("Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Atezo", "Atezo")
    )
  )
)

group_filter <- cohortBuilder::filter(
  type = "discrete_text", id = "group", name = "Group", variable = "group", dataset = "patients"
)
gender_filter <- cohortBuilder::filter(
  type = "discrete", id = "gender", name = "Gender", variable = "gender",
  dataset = "patients", value = "M"
)
age_filter <- cohortBuilder::filter(
  type = "range", id = "age", name = "Age", variable = "age", dataset = "patients", range = NA
)
treatment_filter <- cohortBuilder::filter(
  type = "discrete", id = "treatment", name = "Treatment", variable = "treatment",
  dataset = "therapy", value = "Atezo", gui_input = "vs"
)
visit_filter <- cohortBuilder::filter(
  "date_range", name = "Visit", variable = "visit", dataset = "patients"
)

pin_controlbar <- function(controlbar, ...) {
  controlbar[[2]] <- controlbar[[2]] %>%
    shiny::tagAppendAttributes(`data-pin` = "pin")
  return(controlbar)
}

shiny::runApp(list(
  ui = dashboardPage(
    skin = "light",
    dashboardHeader(title = "Basic dashboard"),
    controlbar = dashboardControlbar(
      shinyCohortBuilder::cb_ui(id = "fk"),
      collapsed = FALSE,
      skin = "light"
    ) %>% pin_controlbar(),
    dashboardSidebar(),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(shiny::verbatimTextOutput("fk_obj"))
      )
    )
  ),
  server = function(input, output, session) {
    coh <- cohortBuilder::cohort(
      raw_source,
      cohortBuilder::step(
        group_filter#,
        # gender_filter,
        # age_filter,
        # treatment_filter,
        # visit_filter#,
        #biom_filter
      )
    )
    # or below if initialized without default data source
    # coh <- cohortBuilder::cohort()

    shinyCohortBuilder::cb_server(id = "fk", coh, feedback = TRUE)

    # input[["fk-cb_data_updated"]] triggers when any data was updated
    # "fk-" prefix is the id passed to cb_ui and cb_server (useful when many cohortBuilder objects are created)
    returned_data <- shiny::eventReactive(input[["fk-cb_data_updated"]], {
      coh$get_data(step_id = coh$last_step_id(), state = "post")
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    output$fk_obj <- shiny::renderPrint({
      print(returned_data())
    })
  }
))
