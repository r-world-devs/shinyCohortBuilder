test_that(".render_filters.tblist groups filters by dataset", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      iris = iris,
      mtcars = mtcars
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris",
      variable = "Species", value = "setosa"
    ),
    cohortBuilder::filter(
      "range", id = "mpg", dataset = "mtcars",
      variable = "mpg", range = c(15, 25)
    )
  ) |> cohortBuilder::run()

  coh$attributes$session <- list(
    ns = function(x) x,
    userData = list(rendered_filters = character(0)),
    input = list(), output = list()
  )
  coh$attributes$stats <- c("pre", "post")
  coh$attributes$feedback <- FALSE
  coh$attributes$show_help <- FALSE

  ui <- .render_filters(coh$get_source(), coh, "1", ns = function(x) x)
  rendered <- as.character(ui)

  # Should have dataset groups
  expect_true(grepl("cb_filters_group", rendered))
  expect_true(grepl("iris", rendered))
  expect_true(grepl("mtcars", rendered))
})

test_that("autofilter.tblist generates filters from data structure", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(
        num_col = c(1, 2, 3),
        cat_col = factor(c("A", "B", "A")),
        date_col = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01"))
      )
    )
  )

  result <- cohortBuilder::autofilter(source)

  # autofilter with "step" (default) adds a step to the source
  # We can check by creating a cohort from it
  coh <- cohortBuilder::cohort(result)
  step <- coh$get_step("1")

  expect_true(length(step$filters) >= 3)

  # Check filter types match column types
  filter_types <- vapply(step$filters, function(f) f@type, character(1))
  expect_true("range" %in% filter_types) # numeric
  expect_true("discrete" %in% filter_types) # factor
  expect_true("date_range" %in% filter_types) # date
})

test_that("autofilter.tblist with attach_as='meta' stores as available_filters", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")), y = 1:2)
    )
  )

  result <- cohortBuilder::autofilter(source, attach_as = "meta")

  expect_true(length(result$available_filters) >= 2)
})

test_that("autofilter.tblist creates discrete_text for unique character columns", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(
        id = c("id1", "id2", "id3"),
        stringsAsFactors = FALSE
      )
    )
  )

  result <- cohortBuilder::autofilter(source)
  coh <- cohortBuilder::cohort(result)
  step <- coh$get_step("1")

  id_filter <- step$filters[[1]]
  expect_equal(id_filter@type, "discrete_text")
})

test_that("autofilter.tblist uses vs gui_input for >3 unique character values", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(
        grp = c("A", "B", "C", "D", "A"),
        stringsAsFactors = FALSE
      )
    )
  )

  result <- cohortBuilder::autofilter(source)
  coh <- cohortBuilder::cohort(result)
  step <- coh$get_step("1")

  grp_filter <- step$filters[[1]]
  expect_equal(grp_filter@type, "discrete")
  expect_equal(cohortBuilder::get_filter_params(grp_filter, "gui_input"), "vs")
})

test_that(".available_filters_choices.tblist returns prepared choices", {
  range_filter <- cohortBuilder::filter(
    "range", id = "copies", name = "Copies",
    dataset = "books", variable = "copies", range = c(1, 5)
  )
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      books = data.frame(copies = 1:5)
    ),
    available_filters = list(range_filter)
  )
  coh <- cohortBuilder::cohort(source) |> cohortBuilder::run()

  # available_filters are read from the source, so no cohort-side seeding needed.
  choices <- .available_filters_choices(coh$get_source(), coh)
  expect_true(length(choices) > 0)
})
