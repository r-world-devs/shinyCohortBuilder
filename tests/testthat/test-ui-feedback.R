library(shinytest2)

# ── Feedback plots ──────────────────────────────────────────────────────────

test_that("Feedback app: filter-level feedback=TRUE shows plot", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-filter-true",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Gender filter has feedback=TRUE
  gender_html <- app$get_html("#coh-1-gender")
  expect_match(gender_html, "cb_feedback", fixed = TRUE)
})

test_that("Feedback app: filter-level feedback=FALSE hides plot", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-filter-false",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Age filter has feedback=FALSE explicitly — no cb_feedback div
  age_html <- app$get_html("#coh-1-age")
  expect_false(grepl("cb_feedback", age_html))
})

test_that("Feedback app: cohort-level feedback=TRUE inherits to filter without override", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-cohort-inherit",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Group filter has no feedback param — inherits from cohort (TRUE)
  group_html <- app$get_html("#coh-1-group")
  expect_match(group_html, "cb_feedback", fixed = TRUE)
})

test_that("Feedback app: changing filter value updates plots", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-update",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  feedback_before <- app$get_html("#coh-1-gender .cb_feedback")

  # Toggle gender selection
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
  app$wait_for_idle(timeout = 10000)

  # Feedback plot SVG should have updated
  feedback_after <- app$get_html("#coh-1-gender .cb_feedback")
  expect_false(identical(feedback_before, feedback_after))
})

test_that("Feedback app: deactivating filter removes feedback content", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-deactivate",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  # Deactivate gender filter
  app$run_js(
    "var cb = document.querySelector('#coh-1-gender .cb_activate_filter input[type=\"checkbox\"]');
     if (cb && cb.checked) {
       cb.checked = false;
       cb.dispatchEvent(new Event('change', {bubbles: true}));
     }"
  )
  app$wait_for_idle(timeout = 5000)

  # Filter content should be hidden
  gender_html <- app$get_html("#coh-1-gender")
  expect_match(gender_html, "hidden-input", fixed = TRUE)
})

# ── Screenshots ─────────────────────────────────────────────────────────────

test_that("Feedback app initial screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "feedback")
  app <- AppDriver$new(
    app_dir, name = "feedback-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(1)
  app$expect_screenshot(threshold = 5)
})
