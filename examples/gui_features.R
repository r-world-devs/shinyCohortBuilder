library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)
options("cb_active_filter" = FALSE)
options("tibble.print_max" = 3)
options("tibble.print_min" = 3)
#options("scb_minified" = FALSE)
librarian$books$copies[1:3] <- NA
librarian_source <- set_source(
  as.tblist(librarian)
)

librarian_cohort <- cohort(
  librarian_source,
  filter(
    "range", id = "copies", dataset = "books",
    variable = "copies", range = NA, keep_na = FALSE,
    active = FALSE
  ),
  filter(
    "discrete",
    id = "program",
    dataset = "borrowers",
    variable = "program",
    stats = "pre"
  )
)

# debug(librarian_cohort$get_filter("1", "copies")$filter_data)
# run(librarian_cohort)
#
#
# librarian_cohort$get_data("1", state = "post")
# librarian_cohort$get_code()

gui(librarian_cohort, attrition = TRUE, steps = TRUE, state = TRUE, code = TRUE, run_button = "global")
