library(shinytest2)

# For tblist source, stats are rendered per-dataset with ID: {ns}-{step_id}-stats_{dataset}
# e.g. "coh-1-stats_patients"

# ── stats = c("pre", "post") ─────────────────────────────────────────────

test_that("Stats pre+post: both pre and post stats are displayed", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "stats-pre-post")
  app <- AppDriver$new(
    app_dir, name = "stats-pre-post",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  stats_html <- app$get_html("#coh-1-stats_patients")
  expect_true(!is.null(stats_html) && nchar(stats_html) > 0)
  # Post stats use cb_delayed class
  expect_match(stats_html, "cb_delayed", fixed = TRUE)
  # Should contain numeric values (row counts)
  expect_match(stats_html, "\\d+")
})

test_that("Stats pre+post: step stats show pre/post separator", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "stats-pre-post")
  app <- AppDriver$new(
    app_dir, name = "stats-pre-post-sep",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  stats_html <- app$get_html("#coh-1-stats_patients")
  # Pre/post separator " / " should be present
  expect_match(stats_html, "/", fixed = TRUE)
})

# ── stats = "pre" ────────────────────────────────────────────────────────

test_that("Stats pre only: shows pre stats without cb_delayed", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "stats-pre")
  app <- AppDriver$new(
    app_dir, name = "stats-pre-only",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  stats_html <- app$get_html("#coh-1-stats_patients")
  expect_true(!is.null(stats_html) && nchar(stats_html) > 0)
  # Pre-only mode should NOT have cb_delayed spans
  expect_false(grepl("cb_delayed", stats_html))
  # Should still contain numeric values
  expect_match(stats_html, "\\d+")
})

# ── stats = "post" ───────────────────────────────────────────────────────

test_that("Stats post only: shows post stats with cb_delayed", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "stats-post")
  app <- AppDriver$new(
    app_dir, name = "stats-post-only",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  stats_html <- app$get_html("#coh-1-stats_patients")
  expect_true(!is.null(stats_html) && nchar(stats_html) > 0)
  # Post-only mode should have cb_delayed spans
  expect_match(stats_html, "cb_delayed", fixed = TRUE)
  # No separator since only one stat type
  expect_false(grepl(" / ", stats_html, fixed = TRUE))
})

# ── stats = NULL ─────────────────────────────────────────────────────────

test_that("Stats NULL: no stats content rendered", {
  skip_on_cran()
  skip_on_ci()
  skip_if_screenshot_only()

  app_dir <- test_path("apps", "stats-none")
  app <- AppDriver$new(
    app_dir, name = "stats-none",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  stats_html <- app$get_html("#coh-1-stats_patients")
  # With stats=NULL, no stat values should be rendered
  if (!is.null(stats_html)) {
    expect_false(grepl("cb_delayed", stats_html))
  }
})

# ── Screenshots ─────────────────────────────────────────────────────────

test_that("Stats pre+post screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "stats-pre-post")
  app <- AppDriver$new(
    app_dir, name = "stats-pre-post-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})

test_that("Stats none screenshot", {
  skip_on_cran()
  skip_on_ci()
  skip_without_screenshot_tests()

  app_dir <- test_path("apps", "stats-none")
  app <- AppDriver$new(
    app_dir, name = "stats-none-screenshot",
    height = 900, width = 1200,
    variant = platform_variant(),
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  app$run_js(disable_animations_js)
  Sys.sleep(0.5)
  app$expect_screenshot(threshold = 3)
})
