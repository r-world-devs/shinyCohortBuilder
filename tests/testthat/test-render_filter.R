test_that(".render_filter creates filter container with correct attributes", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B", "C")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_rf", dataset = "df",
      variable = "x", value = "A"
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

  filter <- coh$get_filter("1", "x_rf")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)

  # .render_filter returns a div with class cb_filter directly
  expect_true(grepl("cb_filter", htmltools::tagGetAttribute(ui, "class")))

  # Should have correct filter_id data attribute
  expect_equal(
    htmltools::tagGetAttribute(ui, "data-filter_id"),
    "x_rf"
  )
})

test_that(".render_filter includes activation switch", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_sw", dataset = "df",
      variable = "x", value = "A"
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

  filter <- coh$get_filter("1", "x_sw")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)
  rendered <- as.character(ui)

  expect_true(grepl("cb_activate_filter", rendered))
  # shinyWidgets prettySwitch renders with "pretty" class
  expect_true(grepl("pretty", rendered))
})

test_that(".render_filter with active=FALSE applies hidden-input class", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_hidden", dataset = "df",
      variable = "x", value = "A", active = FALSE
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

  filter <- coh$get_filter("1", "x_hidden")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)
  rendered <- as.character(ui)

  expect_true(grepl("hidden-input", rendered))
})

test_that(".render_filter with active=TRUE does not apply hidden-input", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_active", dataset = "df",
      variable = "x", value = "A", active = TRUE
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

  filter <- coh$get_filter("1", "x_active")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)
  rendered <- as.character(ui)

  expect_false(grepl("hidden-input", rendered))
})

test_that(".render_filter shows help icon when show_help is TRUE and description exists", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_help", dataset = "df",
      variable = "x", value = "A",
      description = "Help text for filter"
    )
  ) |> cohortBuilder::run()

  coh$attributes$session <- list(
    ns = function(x) x,
    userData = list(rendered_filters = character(0)),
    input = list(), output = list()
  )
  coh$attributes$stats <- c("pre", "post")
  coh$attributes$feedback <- FALSE
  coh$attributes$show_help <- TRUE

  filter <- coh$get_filter("1", "x_help")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)
  rendered <- as.character(ui)

  expect_true(grepl("filter_tooltip", rendered))
})

test_that(".render_filter hides help icon when show_help is FALSE", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_nohelp", dataset = "df",
      variable = "x", value = "A",
      description = "Help text"
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

  filter <- coh$get_filter("1", "x_nohelp")
  filter@extra$gui <- .gui_filter(filter)

  ui <- .render_filter(filter, step_id = "1", cohort = coh, ns = function(x) x)
  rendered <- as.character(ui)

  expect_false(grepl("filter_tooltip", rendered))
})

test_that(".render_filters.default renders stats output and filter containers", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_def", dataset = "df",
      variable = "x", value = "A"
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

  # Use the default method explicitly
  ui <- shinyCohortBuilder:::.render_filters.default(
    source, coh, "1", ns = function(x) x
  )
  rendered <- as.character(ui)

  expect_true(grepl("scb_data_stats", rendered))
  expect_true(grepl("cb_filters", rendered))
})
