test_that("demo_app returns shinyApp object with run_app = FALSE", {
  app_obj <- demo_app(run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app accepts all feature flags", {
  app_obj <- demo_app(
    steps = TRUE, stats = c("pre", "post"),
    run_button = "none", feedback = TRUE,
    state = TRUE, code = TRUE, attrition = TRUE,
    show_help = TRUE, new_step = "clone",
    manage_step = FALSE,
    run_app = FALSE
  )
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with run_button='local' creates app", {
  app_obj <- demo_app(run_button = "local", run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with run_button='global' creates app", {
  app_obj <- demo_app(run_button = "global", run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with steps=FALSE creates app", {
  app_obj <- demo_app(steps = FALSE, run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with all features disabled creates app", {
  app_obj <- demo_app(
    state = FALSE, code = FALSE, attrition = FALSE,
    show_help = FALSE, steps = FALSE, feedback = FALSE,
    run_app = FALSE
  )
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with new_step='configure' creates app", {
  app_obj <- demo_app(new_step = "configure", run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with manage_step=TRUE creates app", {
  app_obj <- demo_app(manage_step = TRUE, run_app = FALSE)
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("demo_app with stats variations creates app", {
  for (stat in list("pre", "post", c("pre", "post"), NULL)) {
    app_obj <- demo_app(stats = stat, run_app = FALSE)
    expect_s3_class(app_obj, "shiny.appobj")
  }
})

test_that("demo_app rejects logical run_button (deprecated)", {
  expect_error(demo_app(run_button = TRUE, run_app = FALSE))
  expect_error(demo_app(run_button = FALSE, run_app = FALSE))
})
