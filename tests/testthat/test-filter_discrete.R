test_that(".gui_filter.discrete returns correct structure", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(iris = iris)
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris",
      variable = "Species", value = "setosa"
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "species")
  gui <- .gui_filter(filter)

  expect_type(gui, "list")
  expect_true("input" %in% names(gui))
  expect_true("feedback" %in% names(gui))
  expect_true("server" %in% names(gui))
  expect_true("update" %in% names(gui))
  expect_true("post_stats" %in% names(gui))
  expect_true("multi_input" %in% names(gui))

  expect_type(gui$input, "closure")
  expect_type(gui$feedback, "closure")
  expect_type(gui$server, "closure")
  expect_type(gui$update, "closure")
  expect_false(gui$multi_input)
})

test_that(".gui_filter.discrete input renders checkbox group by default", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B", "C")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_filter", dataset = "df",
      variable = "x", value = "A"
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  coh$attributes$feedback <- TRUE
  filter <- coh$get_filter("1", "x_filter")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-x_filter", coh)
  rendered <- as.character(ui)

  # Should contain checkbox group with the values
  expect_true(grepl("A", rendered))
  expect_true(grepl("B", rendered))
  expect_true(grepl("C", rendered))
  # Should contain keep_na input
  expect_true(grepl("keep_na", rendered))
})

test_that(".gui_filter.discrete with gui_input='vs' uses virtualSelect", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B", "C", "D", "E")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_vs", dataset = "df",
      variable = "x", value = "A", gui_input = "vs"
    )
  ) |> cohortBuilder::run()

  coh$attributes$stats <- c("pre", "post")
  filter <- coh$get_filter("1", "x_vs")
  filter@extra$gui <- .gui_filter(filter)

  ui <- filter@extra$gui$input("1-x_vs", coh)
  rendered <- as.character(ui)

  # virtualSelect adds specific class
  expect_true(grepl("virtual-select", rendered))
})

test_that(".gui_filter.discrete feedback returns girafe output", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = factor(c("A", "B", "C")))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_fb", dataset = "df",
      variable = "x", value = "A"
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "x_fb")
  gui <- .gui_filter(filter)

  fb <- gui$feedback("1-x_fb", coh, empty = FALSE)
  expect_true("plot_id" %in% names(fb))
  expect_true("output_fun" %in% names(fb))
  expect_true("render_fun" %in% names(fb))
  expect_equal(fb$output_fun, ggiraph::girafeOutput)
})

test_that(".keep_na_input renders checkbox with missing count", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = c("A", "B", NA, "A"))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_na", dataset = "df",
      variable = "x", value = "A"
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "x_na")
  na_input <- .keep_na_input("test_id", filter, coh)
  rendered <- as.character(na_input)

  expect_true(grepl("Keep missing values", rendered))
  expect_true(grepl("1", rendered)) # 1 missing value
  expect_true(grepl("keep_na", rendered))
  expect_true(grepl("cb_na_input", rendered))
})

test_that(".keep_na_input with custom msg_fun formats message", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(
      df = data.frame(x = c("A", NA, NA))
    )
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "x_custom", dataset = "df",
      variable = "x", value = "A"
    )
  ) |> cohortBuilder::run()

  filter <- coh$get_filter("1", "x_custom")
  na_input <- .keep_na_input(
    "test_id", filter, coh,
    msg_fun = function(x) paste("NAs:", x)
  )
  rendered <- as.character(na_input)
  expect_true(grepl("NAs: 2", rendered))
})
