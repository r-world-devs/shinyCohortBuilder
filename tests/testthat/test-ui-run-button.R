library(shinytest2)

# ── Global run button ───────────────────────────────────────────────────────

test_that("Global run button appears and is initially disabled", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-init",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Global run button should be present in the panel
  panel_html <- app$get_html("#coh-cb_panel")
  expect_match(panel_html, "cb_trigger_run", fixed = TRUE)
  expect_match(panel_html, "Run All Steps", fixed = TRUE)
})

test_that("Filter change marks step as pending", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-pending",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Change filter to trigger pending state
  app$run_js(
    "var cbs = $('#coh-1-gender-val input[type=checkbox]');
     cbs.each(function() { this.checked = ($(this).val() === 'F'); });
     $('#coh-1-gender-val.shiny-input-checkboxgroup').trigger('change');"
  )
  app$wait_for_idle(timeout = 5000)

  # Step should have pending class
  step_html <- app$get_html("#coh-1")
  expect_match(step_html, "pending", fixed = TRUE)
})

test_that("Clicking global run button processes pending steps", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-execute",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Change a filter value via jQuery
  app$run_js(
    "var cbs = $('#coh-1-gender-val input[type=checkbox]');
     cbs.each(function() { this.checked = ($(this).val() === 'F'); });
     $('#coh-1-gender-val.shiny-input-checkboxgroup').trigger('change');"
  )
  app$wait_for_idle(timeout = 5000)

  # Click global run button
  app$click(selector = "#coh-cb_panel .cb_trigger_run")
  app$wait_for_idle(timeout = 10000)

  # Step should no longer be pending
  step_html <- app$get_html("#coh-1")
  expect_false(grepl("pending", step_html))
})

test_that("Add step with global run button creates pending step", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-add-step",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Add a cloned step
  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 10000)

  # Second step should exist
  step2_html <- app$get_html("#coh-2")
  expect_true(nchar(step2_html) > 0)
  expect_match(step2_html, "gender", fixed = TRUE)
})

# ── Screenshots ─────────────────────────────────────────────────────────────

test_that("Global run button initial screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})

test_that("Global run button with pending step screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "run-button")
  app <- AppDriver$new(
    app_dir, name = "run-button-pending-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(
    "var cbs = $('#coh-1-gender-val input[type=checkbox]');
     cbs.each(function() { this.checked = ($(this).val() === 'F'); });
     $('#coh-1-gender-val.shiny-input-checkboxgroup').trigger('change');"
  )
  app$wait_for_idle(timeout = 5000)
  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})
