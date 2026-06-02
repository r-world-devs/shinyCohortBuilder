library(shinytest2)

# ── Initial rendering ────────────────────────────────────────────────────────

test_that("Basic app renders panel, steps, and filters", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-init",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Control panel present
  panel <- app$get_html(".cb_panel")
  expect_true(nchar(panel) > 0)

  # Step rendered
  step <- app$get_html(".cb_step")
  expect_true(nchar(step) > 0)

  # Both filters present
  filters_html <- app$get_html(".cb_filters")
  expect_match(filters_html, "gender", fixed = TRUE)
  expect_match(filters_html, "age", fixed = TRUE)

  # Activation switches present
  switches <- app$get_html(".cb_activate_filter")
  expect_true(nchar(switches) > 0)

  # Data output rendered
  output <- app$get_html("#datasets")
  expect_true(nchar(output) > 0)
})

# ── Applying discrete filter values ─────────────────────────────────────────

test_that("Discrete filter: toggling checkbox updates data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-discrete-toggle",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Initial state: gender = "M" selected
  output_before <- app$get_html("#datasets")

  # Uncheck "M" and check "F" via JS
  app$run_js(
    "var checkboxes = document.querySelectorAll('#coh-1-gender input[type=\"checkbox\"]');
     checkboxes.forEach(function(cb) {
       var newChecked = (cb.value === 'F');
       if (cb.checked !== newChecked) {
         cb.checked = newChecked;
         cb.dispatchEvent(new Event('change', {bubbles: true}));
       }
     });"
  )
  app$wait_for_idle(timeout = 5000)

  # Data should have changed
  output_after <- app$get_html("#datasets")
  expect_false(identical(output_before, output_after))
})

# ── Deactivating filter ─────────────────────────────────────────────────────

test_that("Toggling filter active switch hides content", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-deactivate",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Deactivate gender filter by clicking the prettySwitch
  app$run_js(
    "var cb = document.querySelector('#coh-1-gender .cb_activate_filter input[type=\"checkbox\"]');
     if (cb && cb.checked) {
       cb.checked = false;
       cb.dispatchEvent(new Event('change', {bubbles: true}));
     }"
  )
  app$wait_for_idle(timeout = 5000)

  # The filter content should be hidden
  filter_html <- app$get_html("#coh-1-gender")
  expect_match(filter_html, "hidden-input", fixed = TRUE)
})

# ── Cloning step ────────────────────────────────────────────────────────────

test_that("Add step (clone) creates second step", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-add-step",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Initially one step
  steps_html <- app$get_html(".cb_steps")
  n_steps_before <- lengths(regmatches(steps_html, gregexpr("cb_step", steps_html)))

  # Click add step button
  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 10000)

  # Now two steps
  steps_html <- app$get_html(".cb_steps")
  n_steps_after <- lengths(regmatches(steps_html, gregexpr("cb_step", steps_html)))
  expect_gt(n_steps_after, n_steps_before)

  # Second step should have same filters as first
  step2_html <- app$get_html("#coh-2")
  expect_match(step2_html, "gender", fixed = TRUE)
  expect_match(step2_html, "age", fixed = TRUE)
})

# ── Removing step ───────────────────────────────────────────────────────────

test_that("Remove step removes second step", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-rm-step",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Add a step first
  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 10000)

  step2_exists <- tryCatch({
    nchar(app$get_html("#coh-2")) > 0
  }, error = function(e) FALSE)
  expect_true(step2_exists)

  # Remove the second step via the delete button
  app$click(selector = "#coh-2 .cb_rm_step")
  app$wait_for_idle(timeout = 5000)

  # Second step should be gone
  steps_html <- app$get_html(".cb_steps")
  expect_false(grepl("data-step_id=\"2\"", steps_html, fixed = TRUE))
})

# ── Clear filters ───────────────────────────────────────────────────────────

test_that("Clear filters resets filter values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-clear",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  output_before <- app$get_html("#datasets")

  # Click the clear filters button (sync icon in step header)
  app$click(selector = "#coh-1 .panel-heading button[title='Clear Filters']")
  app$wait_for_idle(timeout = 5000)

  # Data should change because filters were reset to select all
  output_after <- app$get_html("#datasets")
  expect_false(identical(output_before, output_after))
})

# ── State modal ─────────────────────────────────────────────────────────────

test_that("Get State modal shows JSON state", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-get-state",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Click Get State button
  app$click(selector = "#coh-cb_panel button:has(> .fa-stream)")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Modal should appear with JSON state
  modal_html <- app$get_html(".modal-dialog")
  expect_match(modal_html, "Cohort state", fixed = TRUE)
})

# ── Reproducible code modal ────────────────────────────────────────────────

test_that("Show Reproducible Code modal displays code", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-repro-code",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Click Show Reproducible Code button
  app$click(selector = "#coh-cb_panel button:has(> .fa-code)")
  app$wait_for_idle(timeout = 5000)
  Sys.sleep(0.5)

  # Modal should appear with code
  modal_html <- app$get_html(".modal-dialog")
  expect_match(modal_html, "Reproducible code", fixed = TRUE)
  expect_match(modal_html, "scb-reproducible-code", fixed = TRUE)
})

# ── Screenshots ─────────────────────────────────────────────────────────────

test_that("Basic app initial screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})

test_that("Basic app with two steps screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "basic")
  app <- AppDriver$new(
    app_dir, name = "basic-two-steps-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "#coh-cb_panel .cb_add_step")
  app$wait_for_idle(timeout = 10000)
  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})
