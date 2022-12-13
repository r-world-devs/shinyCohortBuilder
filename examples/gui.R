library(magrittr)
library(cohortBuilder)
library(shinyCohortBuilder)
coh <- cohort(
  source = set_source(
    tblist(
      iris = iris, mtcars = mtcars,
      set = data.frame(name = c("setosa", "virginica"))
    ),
    binding_keys = bind_keys(
      bind_key(data_key("set", "name"), data_key("iris", "Species"))
    )
  )
) %>%
  add_filter(
    filter("discrete", id = "species", dataset = "iris", variable = "Species", value = c("setosa", "versicolor")),
    step_id = "1"
  ) %>%
  add_filter(
    filter("discrete", dataset = "set", variable = "name", value = "setosa"),
    step_id = "2"
  )
gui(coh)
