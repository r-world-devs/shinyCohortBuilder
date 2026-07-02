test_that("scb_labels contains all required labels", {
  required_labels <- c(
    "run_steps_global", "get_state", "set_state",
    "show_attrition", "show_repro_code", "add_step",
    "delete_step_title", "clear_filters_title",
    "run_single_step_title", "show_edit_title",
    "keep_missing", "step",
    "filter_discrete_text_bttn_label",
    "filter_query_bttn_label",
    "filter_show_query_bttn_label",
    "manage_step"
  )

  for (label in required_labels) {
    expect_true(
      label %in% names(scb_labels),
      info = paste("Missing label:", label)
    )
    expect_type(scb_labels[[label]], "character")
  }
})

test_that("scb_icons contains all required icons", {
  required_icons <- c(
    "run_steps_global", "get_state", "set_state",
    "show_attrition", "show_repro_code", "add_step",
    "delete_step", "clear_filters",
    "run_single_step", "show_edit", "filter_help",
    "filter_discrete_text_bttn_icon",
    "filter_query_bttn_icon",
    "filter_show_query_bttn_icon",
    "dataset_help_icon",
    "manage_step"
  )

  for (icon_name in required_icons) {
    expect_true(
      icon_name %in% names(scb_icons),
      info = paste("Missing icon:", icon_name)
    )
    expect_s3_class(scb_icons[[icon_name]], "shiny.tag")
  }
})

test_that("scb_chart_palette has required color elements", {
  expect_true("discrete" %in% names(scb_chart_palette))
  expect_true("histogram" %in% names(scb_chart_palette))
  expect_true("no_data" %in% names(scb_chart_palette))

  expect_true(length(scb_chart_palette$discrete) >= 2)
  expect_type(scb_chart_palette$histogram, "character")
  expect_type(scb_chart_palette$no_data, "character")
})

test_that("scb_labels values are non-empty strings", {
  for (label_name in names(scb_labels)) {
    expect_true(
      nchar(scb_labels[[label_name]]) > 0,
      info = paste("Empty label:", label_name)
    )
  }
})

test_that("scb_chart_palette discrete colors are valid hex", {
  for (color in scb_chart_palette$discrete) {
    expect_true(
      grepl("^#[0-9a-fA-F]{6}$", color),
      info = paste("Invalid hex color:", color)
    )
  }
})
