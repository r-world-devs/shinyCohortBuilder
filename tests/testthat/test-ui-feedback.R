library(shinytest2)

# ── Feedback plots ──────────────────────────────────────────────────────────

test_that("Feedback app: filter-level feedback=TRUE shows plot", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- cb_app_driver(
    app_dir, name = "feedback-filter-true"
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 10000)

  # Gender filter has feedback=TRUE
  gender_html <- app$get_html(".cb_filter[data-filter_id='gender']")
  expect_match(gender_html, "cb_feedback", fixed = TRUE)
})

test_that("Feedback app: filter-level feedback=FALSE hides plot", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- cb_app_driver(
    app_dir, name = "feedback-filter-false"
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 10000)

  # Age filter has feedback=FALSE explicitly — no cb_feedback div
  age_html <- app$get_html(".cb_filter[data-filter_id='age']")
  expect_false(grepl("cb_feedback", age_html))
})

test_that("Feedback app: cohort-level feedback=TRUE inherits to filter without override", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- cb_app_driver(
    app_dir, name = "feedback-cohort-inherit"
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 10000)

  # Group filter has no feedback param — inherits from cohort (TRUE)
  group_html <- app$get_html(".cb_filter[data-filter_id='group']")
  expect_match(group_html, "cb_feedback", fixed = TRUE)
})

test_that("Feedback app: changing filter value updates plots", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "feedback")
  app <- cb_app_driver(
    app_dir, name = "feedback-update"
  )
  on.exit(app$stop(), add = TRUE)

  feedback_before <- app$get_html("#coh-1-gender .cb_feedback")

  # Toggle gender selection via jQuery
  app$run_js(
    "var cbs = $('#coh-1-gender-val input[type=checkbox]');
     cbs.each(function() { this.checked = ($(this).val() === 'F'); });
     $('#coh-1-gender-val.shiny-input-checkboxgroup').trigger('change');"
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
  app <- cb_app_driver(
    app_dir, name = "feedback-deactivate"
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle(timeout = 10000)

  # Deactivate gender filter via jQuery trigger
  app$run_js(
    "var el = document.getElementById('coh-active_1-gender');
     el.checked = false;
     $(el).trigger('change');"
  )
  app$wait_for_idle(timeout = 15000)

  # Filter content should be hidden
  gender_html <- app$get_html(".cb_filter[data-filter_id='gender']")
  expect_match(gender_html, "hidden-input", fixed = TRUE)
})

# ── Screenshots ─────────────────────────────────────────────────────────────

test_that("Feedback app initial screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "feedback")
  app <- cb_app_driver(
    app_dir, name = "feedback-screenshot"
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(1)
  app$expect_screenshot(threshold = 5)
})
