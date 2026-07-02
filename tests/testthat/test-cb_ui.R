# Helper to wrap tagList output in a div for tagQuery
wrap_ui <- function(ui) {
  htmltools::tagQuery(shiny::div(ui))
}

test_that("cb_ui renders complete container structure", {
  ui <- cb_ui("test_id")
  tag <- wrap_ui(ui)

  # Container with correct namespace
  container <- tag$find(".cb_container")$selectedTags()
  expect_length(container, 1)
  expect_equal(
    htmltools::tagGetAttribute(container[[1]], "data-ns_prefix"),
    "test_id-"
  )

  # Panel and accordion
  expect_length(tag$find(".cb_panel")$selectedTags(), 1)
  expect_length(tag$find(".cb_steps")$selectedTags(), 1)
})

test_that("cb_ui applies namespace to element IDs", {
  ui <- cb_ui("mymod")
  tag <- wrap_ui(ui)

  container <- tag$find(".cb_container")$selectedTags()
  expect_equal(htmltools::tagGetAttribute(container[[1]], "id"), "mymod-cb_container")

  panel <- tag$find(".cb_panel")$selectedTags()
  expect_equal(htmltools::tagGetAttribute(panel[[1]], "id"), "mymod-cb_panel")
})

test_that("cb_ui includes CSS and JS assets in head", {
  ui <- cb_ui("t")
  tag <- wrap_ui(ui)

  scripts <- tag$find("script")$selectedTags()
  expect_true(length(scripts) >= 1)

  links <- tag$find("link")$selectedTags()
  expect_true(length(links) >= 1)
})

test_that("cb_ui control buttons are present by default", {
  ui <- cb_ui("t", state = TRUE, code = TRUE, attrition = TRUE)
  rendered <- as.character(ui)

  expect_true(grepl("Set State", rendered))
  expect_true(grepl("Get State", rendered))
  expect_true(grepl("Show Reproducible Code", rendered))
  expect_true(grepl("Show Attrition Data", rendered))
  expect_true(grepl("Add Step", rendered))
})

test_that("cb_ui hides state buttons when state = FALSE", {
  ui <- cb_ui("t", state = FALSE)
  tag <- wrap_ui(ui)

  state_div <- tag$find(".cb_no_state")$selectedTags()
  expect_length(state_div, 1)
})

test_that("cb_ui hides code button when code = FALSE", {
  ui <- cb_ui("t", code = FALSE)
  rendered <- as.character(ui)
  expect_true(grepl("cb_no_code", rendered))
})

test_that("cb_ui hides attrition button when attrition = FALSE", {
  ui <- cb_ui("t", attrition = FALSE)
  rendered <- as.character(ui)
  expect_true(grepl("cb_no_attrition", rendered))
})

test_that("cb_ui applies cb_no_steps class when steps = FALSE", {
  ui <- cb_ui("t", steps = FALSE)
  tag <- wrap_ui(ui)

  container <- tag$find(".cb_container")$selectedTags()
  expect_true(grepl("cb_no_steps", htmltools::tagGetAttribute(container[[1]], "class")))
})

test_that("cb_ui does not apply cb_no_steps class when steps = TRUE", {
  ui <- cb_ui("t", steps = TRUE)
  tag <- wrap_ui(ui)

  container <- tag$find(".cb_container")$selectedTags()
  expect_false(grepl("cb_no_steps", htmltools::tagGetAttribute(container[[1]], "class")))
})

test_that("cb_ui manage_step = FALSE hides manage button", {
  ui <- cb_ui("t", manage_step = FALSE)
  rendered <- as.character(ui)
  expect_true(grepl("cb_no_manage_step", rendered))
})

test_that("cb_ui manage_step = TRUE shows manage button", {
  ui <- cb_ui("t", manage_step = TRUE)
  rendered <- as.character(ui)
  expect_false(grepl("cb_no_manage_step", rendered))
})

test_that("cb_ui new_step = 'clone' uses add_step action", {
  ui <- cb_ui("t", new_step = "clone")
  rendered <- as.character(ui)
  # The add step button should trigger add_step action
  expect_true(grepl("add_step", rendered))
})

test_that("cb_ui new_step = 'configure' uses add_step_modal action", {
  ui <- cb_ui("t", new_step = "configure")
  rendered <- as.character(ui)
  expect_true(grepl("add_step_modal", rendered))
})

test_that("cb_ui extra attributes passed via ... are applied", {
  ui <- cb_ui("t", style = "width: 300px;")
  tag <- wrap_ui(ui)

  container <- tag$find(".cb_container")$selectedTags()
  expect_equal(htmltools::tagGetAttribute(container[[1]], "style"), "width: 300px;")
})

test_that("cb_ui panel starts disabled", {
  ui <- cb_ui("t")
  tag <- wrap_ui(ui)

  panel <- tag$find(".cb_panel")$selectedTags()
  expect_true(grepl("disabled", htmltools::tagGetAttribute(panel[[1]], "class")))
})
