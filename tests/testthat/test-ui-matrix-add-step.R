library(shinytest2)

# UI matrix tests — ACTION: adding a new step (clone of the last step).
#
# These exercise the cross-product of run_button x propagate_domains x
# render_source x cache and check, on the freshly added step:
#   * choices / ranges (domain narrowing)
#   * pending state
#   * statistics shown
#   * feedback plots
#
# The single parametrized app (apps/matrix) is configured per test via env vars
# (see cb_matrix_driver). Data: group "A" occurs only with gender "M", so once
# step 1 restricts gender to "F" the downstream group domain narrows to {B, C}
# (under data/cache propagation) and the age range narrows to the F-rows' range.

# ── New step inherits filters and choices ────────────────────────────────────

test_that("Add step: new step clones parent filters and choices (auto render)", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-clone-auto",
    list(run_button = "none", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)

  step2 <- app$get_html("#coh-2")
  expect_true(nchar(step2) > 0)
  # All three parent filters carried over.
  expect_match(step2, "gender", fixed = TRUE)
  expect_match(step2, "age", fixed = TRUE)
  expect_match(step2, "group", fixed = TRUE)

  # The cloned step carries the parent filter values; its choices include the
  # parent's selectable values.
  choices <- cb_discrete_choices(app, "2", "group")
  expect_true(all(c("B", "C") %in% choices))
})

# ── Domain on add: new step shows the cloned/declared domain ─────────────────

test_that("Add step: new step shows declared domain on add (narrows only after parent update)", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  # Propagation (filter/cache/data) narrows a downstream step when the *parent*
  # step is updated/re-run, not at the moment of cloning. So immediately after
  # add the new step renders the declared full domain {A,B,C} / {18,80}. The
  # post-update narrowing is asserted in test-ui-matrix-update-filter.R.
  for (mode in c("filter", "data")) {
    app <- cb_matrix_driver(
      paste0("add-domain-", mode),
      list(run_button = "none", propagate = mode,
           render_source = "domain", cache = TRUE, stats = "none")
    )

    cb_add_step(app)

    choices <- cb_discrete_choices(app, "2", "group")
    expect_setequal(choices, c("A", "B", "C"))

    bounds <- cb_range_bounds(app, "2", "age")
    expect_false(is.null(bounds))
    expect_equal(bounds, c(18, 80))

    app$stop()
  }
})

# ── Pending state on add ─────────────────────────────────────────────────────

test_that("Add step: no run button -> new step is not pending", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-none-pending",
    list(run_button = "none", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)
  expect_false(cb_is_pending(app, "2"))
})

test_that("Add step: global run button -> new step is pending until run", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-global-pending",
    list(run_button = "global", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)
  expect_true(cb_is_pending(app, "2"))

  # Running all steps clears the pending state.
  cb_run_all(app)
  expect_false(cb_is_pending(app, "2"))
})

test_that("Add step: local run button -> new step is pending until run", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-local-pending",
    list(run_button = "local", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)
  expect_true(cb_is_pending(app, "2"))

  cb_run_step(app, "2")
  expect_false(cb_is_pending(app, "2"))
})

# ── Statistics on the new step ──────────────────────────────────────────────

test_that("Add step: stats=pre+post renders pre/post stats on new step", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-stats-prepost",
    list(run_button = "none", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)

  stats <- cb_stats_html(app, "2")
  expect_true(!is.null(stats) && nchar(stats) > 0)
  expect_match(stats, "cb_delayed", fixed = TRUE)   # post stats
  expect_match(stats, "\\d+")                         # numeric counts
})

test_that("Add step: stats=NULL renders no stats on new step", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-stats-none",
    list(run_button = "none", propagate = "filter",
         render_source = "domain", cache = FALSE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)

  stats <- cb_stats_html(app, "2")
  if (!is.null(stats)) {
    expect_false(grepl("cb_delayed", stats))
  }
})

# ── Feedback plots on the new step ──────────────────────────────────────────

test_that("Add step: feedback=TRUE shows feedback plots on new step", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-feedback-on",
    list(run_button = "none", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post",
         feedback = TRUE)
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 10000)

  cb_add_step(app)
  app$wait_for_idle(timeout = 10000)

  expect_true(cb_has_feedback(app, "2", "gender"))
})

test_that("Add step: feedback=FALSE shows no feedback plots on new step", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- cb_matrix_driver(
    "add-feedback-off",
    list(run_button = "none", propagate = "filter",
         render_source = "auto", cache = TRUE, stats = "pre+post",
         feedback = FALSE)
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)
  expect_false(cb_has_feedback(app, "2", "gender"))
})

# ── cache = FALSE still renders a usable new step ───────────────────────────

test_that("Add step: cache=FALSE + domain render produces a usable new step", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  # With cache = FALSE the source is not scanned. Domain rendering must still
  # produce a complete, selectable new step from the declared domains.
  app <- cb_matrix_driver(
    "add-nocache-domain",
    list(run_button = "none", propagate = "filter",
         render_source = "domain", cache = FALSE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  cb_add_step(app)
  choices <- cb_discrete_choices(app, "2", "group")
  expect_setequal(choices, c("A", "B", "C"))
  bounds <- cb_range_bounds(app, "2", "age")
  expect_equal(bounds, c(18, 80))
})
