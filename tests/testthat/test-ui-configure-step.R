library(shinytest2)

# ── Configure step mode (new_step="configure") ─────────────────────────────

test_that("Configure step: Add Step opens filter selection modal", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "configure-step-modal",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Click Add Step button (should open modal in configure mode)
  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Modal with filter selection should appear
  modal_html <- app$get_html(".modal-dialog")
  expect_match(modal_html, "Configure new step", fixed = TRUE)
  expect_match(modal_html, "Choose filters", fixed = TRUE)
})

test_that("Configure step: selecting filters and accepting creates step", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "configure-step-create",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Open configure modal
  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Select age and group filters in virtualSelect
  app$run_js(
    "var el = document.getElementById('coh-configure_step');
     if (el && el.virtualSelect) {
       el.virtualSelect.setValue(['age', 'group']);
       $(el).trigger('change');
     }"
  )
  app$wait_for_idle(timeout = 3000)

  # Click Accept to create the step
  app$click(selector = "#coh-add_step_configured")
  app$wait_for_idle(timeout = 10000)

  # Step 2 should exist with the selected filters
  step2_html <- app$get_html("#coh-2")
  expect_true(nchar(step2_html) > 0)
  expect_match(step2_html, "age", fixed = TRUE)
  expect_match(step2_html, "group", fixed = TRUE)
})

# ── Manage step ─────────────────────────────────────────────────────────────

test_that("Manage step: opens modal with current filter selection", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "manage-step-modal",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Click Manage Last Step button
  app$click(selector = "#coh-cb_panel button:has(> .fa-pen-to-square)")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Modal should appear
  modal_html <- app$get_html(".modal-dialog")
  expect_match(modal_html, "Manage last step", fixed = TRUE)
  expect_match(modal_html, "Choose filters", fixed = TRUE)
})

test_that("Manage step: adding filter via manage modal", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "manage-step-add-filter",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Initially only gender filter
  step1_html <- app$get_html("#coh-1")
  expect_match(step1_html, "gender", fixed = TRUE)

  # Open manage modal
  app$click(selector = "#coh-cb_panel button:has(> .fa-pen-to-square)")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Add age filter to selection
  app$run_js(
    "var el = document.getElementById('coh-manage_step');
     if (el && el.virtualSelect) {
       el.virtualSelect.setValue(['gender', 'age']);
       $(el).trigger('change');
     }"
  )
  app$wait_for_idle(timeout = 3000)

  # Accept changes
  app$click(selector = "#coh-manage_step_configured")
  app$wait_for_idle(timeout = 10000)

  # Step should now have both filters
  step1_html <- app$get_html("#coh-1")
  expect_match(step1_html, "gender", fixed = TRUE)
  expect_match(step1_html, "age", fixed = TRUE)
})

test_that("Manage step: removing filter via manage modal", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "manage-step-rm-filter",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Open manage modal and set only age (removing gender)
  app$click(selector = "#coh-cb_panel button:has(> .fa-pen-to-square)")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  app$run_js(
    "var el = document.getElementById('coh-manage_step');
     if (el && el.virtualSelect) {
       el.virtualSelect.setValue(['age']);
       $(el).trigger('change');
     }"
  )
  app$wait_for_idle(timeout = 3000)

  app$click(selector = "#coh-manage_step_configured")
  app$wait_for_idle(timeout = 10000)

  # Gender filter should be removed
  step1_html <- app$get_html("#coh-1")
  expect_match(step1_html, "age", fixed = TRUE)
  expect_false(grepl("data-filter_id=\"gender\"", step1_html))
})

# ── Screenshots ─────────────────────────────────────────────────────────────

test_that("Configure step initial screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "configure-step-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})

test_that("Configure step modal screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "configure-step")
  app <- AppDriver$new(
    app_dir, name = "configure-step-modal-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  app$run_js(disable_animations_js)
  Sys.sleep(0.3)
  app$expect_screenshot(threshold = 3)
})
