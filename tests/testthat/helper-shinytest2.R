# Shared helpers for shinytest2 UI tests

# Construct an AppDriver and block until the cohortBuilder panel has finished its
# initial reactive render. AppDriver$new() only waits for the Shiny app to load,
# not for the filter panel which is rendered reactively after init. Querying the
# DOM before that render completes returns NULL / partial HTML and makes tests
# flaky. Waiting for the `.cb_panel` element (and then for idle) makes the
# subsequent DOM assertions deterministic.
cb_app_driver <- function(app_dir, name, ...,
                          ready_selector = ".cb_panel",
                          ready_timeout = 30000) {
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = name,
    height = 900,
    width = 1200,
    variant = shinytest2::platform_variant(),
    load_timeout = 60000,
    ...
  )

  # Wait until the reactive panel exists in the DOM, then for the app to settle.
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", ready_selector),
    timeout = ready_timeout
  )
  app$wait_for_idle(timeout = ready_timeout)

  app
}

skip_on_ci <- function() {
  if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITLAB_CI"))) {
    testthat::skip("UI tests skipped on CI")
  }
}

skip_without_screenshot_tests <- function() {
  if (!nzchar(Sys.getenv("SCREENSHOT_TESTS"))) {
    testthat::skip("Set SCREENSHOT_TESTS=1 to run screenshot tests")
  }
}

skip_if_screenshot_only <- function() {
  if (nzchar(Sys.getenv("SCREENSHOT_ONLY"))) {
    testthat::skip("SCREENSHOT_ONLY mode — skipping non-screenshot tests")
  }
}

# Disable animations and transitions for deterministic screenshots
disable_animations_js <- paste0(
  "var s = document.createElement('style');",
  "s.textContent = [",
  "  '*, *::before, *::after { animation: none !important; transition: none !important; }',",
  "  '.girafe_container_std svg { display: none !important; }'",
  "].join('\\n');",
  "document.head.appendChild(s);"
)
