library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
options("cb_active_filter" = FALSE)
options("tibble.print_max" = 3)
options("tibble.print_min" = 3)
librarian_source <- set_source(
  as.tblist(librarian)
)
librarian_cohort <- cohort(
  librarian_source,
  filter(
    "range", id = "copies", dataset = "books",
    variable = "copies", range = c(4, 8),
    active = FALSE
  ),
  filter(
    "discrete",
    id = "program",
    dataset = "borrowers",
    variable = "program"
  )
)
gui(librarian_cohort, attrition = TRUE, steps = TRUE, state = FALSE, code = FALSE)
