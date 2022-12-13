library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)
library(shinyQueryBuilder)
options(scb_minified = FALSE)
coh <- cohort(
  source = set_source(
    tblist(iris = iris, mtcars = mtcars)
  )
) %>%
  add_filter(
    filter(
      "query", id = "iris_query", dataset = "iris", variables = c("Petal.Length", "Species"),
      value = queryBuilder::queryGroup(
        queryBuilder::queryRule("Petal.Length", "between", c(2, 4))
      ),
      active = FALSE
    )
  )
gui(coh)
