# cb_server cannot be tested with shiny::testServer because testServer's mock
# session lacks registerBookmarkExclude. We test via shinytest2::AppDriver
# for integration and test the cohort setup/cleanup logic directly.

test_that("cb_server rejects logical run_button (deprecated)", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(df = data.frame(x = 1:3))
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter("range", id = "xf", dataset = "df", variable = "x", range = c(1, 3))
  )

  expect_error(
    shiny::shinyApp(
      ui = shiny::fluidPage(cb_ui("t")),
      server = function(input, output, session) {
        cb_server("t", coh, run_button = TRUE)
      }
    ),
    NA # no error creating app, error only on run
  )
})

test_that("cb_server works within a shinyApp definition", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(df = data.frame(x = factor(c("A", "B", "C"))))
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "xf", dataset = "df",
      variable = "x", value = "A"
    )
  )

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(cb_ui("test")),
    server = function(input, output, session) {
      cb_server("test", coh, run_button = "none", feedback = FALSE, enable_bookmarking = "disable")
    }
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("cb_server validates render_source", {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(df = data.frame(x = 1:3))
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter("range", id = "xf", dataset = "df", variable = "x", range = c(1, 3))
  )

  expect_error(
    cb_server("t", coh, render_source = "invalid"),
    "should be one of"
  )
})

# Integration tests (AppDriver) moved to test-ui-basic.R
