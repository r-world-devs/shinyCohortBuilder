test_that(".gui_filter.range returns correct structure", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(val = c(1, 5, 10, 15, 20))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "range", id = "val_rng", dataset = "df",
      variable = "val", range = c(5, 15)
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "val_rng")
  gui <- .gui_filter(filter)

  expect_type(gui, "list")
  expect_true("input" %in% names(gui))
  expect_true("feedback" %in% names(gui))
  expect_true("update" %in% names(gui))
  expect_false(gui$post_stats)
})

test_that(".gui_filter.range input renders slider and numeric by default", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(val = c(1, 5, 10, 15, 20))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "range", id = "val_def", dataset = "df",
      variable = "val", range = c(5, 15)
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  filter <- coh$get_filter("1", "val_def")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-val_def", coh)
  rendered <- as.character(ui)

  # Should have both slider and numeric range inputs
  expect_true(grepl("slider", rendered))
  expect_true(grepl("numrange", rendered))
  expect_true(grepl("keep_na", rendered))
})

test_that(".gui_filter.range with gui_input='slider' renders only slider", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(val = c(1, 5, 10, 15, 20))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "range", id = "val_sl", dataset = "df",
      variable = "val", range = c(5, 15),
      gui_input = "slider"
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  filter <- coh$get_filter("1", "val_sl")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-val_sl", coh)
  rendered <- as.character(ui)

  expect_true(grepl("slider", rendered))
  expect_false(grepl("numrange", rendered))
})

test_that(".gui_filter.range with gui_input='numeric' renders only numeric", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(val = c(1, 5, 10, 15, 20))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "range", id = "val_num", dataset = "df",
      variable = "val", range = c(5, 15),
      gui_input = "numeric"
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  filter <- coh$get_filter("1", "val_num")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-val_num", coh)
  rendered <- as.character(ui)

  expect_false(grepl("slider", rendered))
  expect_true(grepl("numrange", rendered))
})

test_that(".gui_filter.range feedback returns plotOutput", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(val = 1:20)
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "range", id = "val_fb", dataset = "df",
      variable = "val", range = c(5, 15)
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "val_fb")
  gui <- .gui_filter(filter)

  fb <- gui$feedback("1-val_fb", coh, empty = FALSE)
  expect_equal(fb$output_fun, shiny::plotOutput)
  expect_true(!is.null(fb$render_fun))
})

test_that(".gui_filter.date_range returns correct structure", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(dt = as.Date(c("2020-01-01", "2020-06-15", "2021-01-01")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "date_range", id = "dt_rng", dataset = "df",
      variable = "dt", range = c(as.Date("2020-01-01"), as.Date("2021-01-01"))
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "dt_rng")
  gui <- .gui_filter(filter)

  expect_type(gui, "list")
  expect_false(gui$post_stats)
  expect_false(gui$multi_input)
})

test_that(".gui_filter.date_range input renders dateRangeInput", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(dt = as.Date(c("2020-01-01", "2020-06-15", "2021-01-01")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "date_range", id = "dt_ui", dataset = "df",
      variable = "dt", range = c(as.Date("2020-01-01"), as.Date("2021-01-01"))
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  filter <- coh$get_filter("1", "dt_ui")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-dt_ui", coh)
  rendered <- as.character(ui)

  expect_true(grepl("shiny-date-range-input", rendered))
  expect_true(grepl("keep_na", rendered))
})
