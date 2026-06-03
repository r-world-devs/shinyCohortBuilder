test_that(".cb_input wraps UI with correct class and data-param", {
  input <- .cb_input(
    shiny::numericInput("val", "Value", value = 1),
    data_param = "range"
  )

  expect_equal(htmltools::tagGetAttribute(input, "class"), "cb_input")
  expect_equal(htmltools::tagGetAttribute(input, "data-param"), "range")
  expect_equal(htmltools::tagGetAttribute(input, "data-exec_state"), "init")
})

test_that(".cb_input preserves the inner UI element", {
  inner <- shiny::numericInput("num_test", "Test", value = 5)
  wrapped <- .cb_input(inner, data_param = "value")

  rendered <- as.character(wrapped)
  expect_true(grepl("num_test", rendered))
  expect_true(grepl("Test", rendered))
})

test_that(".cb_input handles different data_param values", {
  for (param in c("value", "range", "keep_na", "active", "values")) {
    input <- .cb_input(shiny::div(), data_param = param)
    expect_equal(htmltools::tagGetAttribute(input, "data-param"), param)
  }
})

test_that(".cb_input with priority sets priority attribute", {
  input <- .cb_input(shiny::div(), data_param = "range", priority = "event")
  expect_equal(htmltools::tagGetAttribute(input, "priority"), "event")
})

test_that(".cb_input without priority has no priority attribute", {
  input <- .cb_input(shiny::div(), data_param = "range")
  expect_null(htmltools::tagGetAttribute(input, "priority"))
})

test_that(".cb_input passes extra attributes", {
  input <- .cb_input(shiny::div(), data_param = "value", style = "display: inline;")
  expect_equal(htmltools::tagGetAttribute(input, "style"), "display: inline;")
})
