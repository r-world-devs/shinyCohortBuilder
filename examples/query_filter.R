library(magrittr)
library(cohortBuilder)
pkgload::load_all()
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
      gui_args = list(filters = list(`Petal.Length` = list(label = "Petal Length")), allow_groups = FALSE),
      active = FALSE
    )
  )
gui(coh, run_button = "global")
