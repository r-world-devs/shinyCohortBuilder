library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
options("cb_active_filter" = FALSE)
options("tibble.print_max" = 3)
options("tibble.print_min" = 3)

librarian_source <- set_source(
  as.tblist(librarian),
  available_filters = list(
    filter(
      "discrete",
      id = "program",
      dataset = "borrowers",
      variable = "program"
    ),
    filter(
      "discrete",
      id = "name",
      dataset = "borrowers",
      variable = "name",
      gui_input = "vs"
    ),
    filter(
      "range",
      id = "registered",
      dataset = "borrowers",
      variable = "registered",
      gui_input = "vs"
    )
  )
)
librarian_cohort <- cohort(
  librarian_source
)

gui(librarian_cohort, new_step = "configure")
