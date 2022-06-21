test_that("Button has correct structure ", {
  test_button <- button("Label", icon = shiny::icon("plus"), type = "test")
  expect_true(grepl("test", htmltools::tagGetAttribute(test_button, "class")))
  expect_true(grepl("scb_button", htmltools::tagGetAttribute(test_button, "class")))
  expect_equal(length(test_button$children), 2)
})

test_that("Panel has correct structure ", {
  heading <- "Heading"
  content <- "Content"
  panel_no_heading <- panel(body = content)
  panel_no_heading_tag <- htmltools::tagQuery(panel_no_heading)
  expect_true(grepl("panel", htmltools::tagGetAttribute(panel_no_heading, "class")))
  expect_true(
    length(panel_no_heading_tag$find(".panel-heading")$selectedTags()) == 0
  )
  expect_true(
    length(panel_no_heading_tag$find(".panel-body")$selectedTags()) == 1
  )
  panel_with_heading <- panel(heading = heading, body = content)
  panel_with_heading_tag <- htmltools::tagQuery(panel_with_heading)
  expect_true(
    length(panel_with_heading_tag$find(".panel-heading")$selectedTags()) == 1
  )
})

test_that("Divider has correct structure ", {
  test_divider <- divider("Label")
  test_divider_tag <- htmltools::tagQuery(test_divider)
  expect_equal(htmltools::tagGetAttribute(test_divider, "class"), "divider")
  expect_equal(length(test_divider$children), 3)
})
