library(shinytest2)

# UI matrix tests — ACTION: updating a filter in a NON-LAST step.
#
# Scenario for every test: a two-step cohort is built (step 1 + a cloned step 2),
# step 1 starts with gender = {F, M} (all rows), then the test updates step 1's
# gender filter to {F} only. Because group "A" occurs only with gender "M",
# restricting step 1 to "F" removes all "A" rows, so the downstream step 2 must
# react. What exactly changes depends on the configuration:
#
#   * propagate = "cache" | "data": step 2's group domain narrows to {B, C} and
#     its age range narrows to the F-rows' range (35-50) when render_source =
#     "domain".
#   * propagate = "none" | "filter": step 2's group domain is unchanged from the
#     upstream perspective ({A, B, C}); filter mode narrows a filter from its own
#     values, not from the upstream step's remaining data.
#
# We also check pending state, statistics and feedback plots react correctly.

# Build a two-step cohort with step 1 gender unrestricted, returning the driver.
setup_two_step <- function(name, config) {
  app <- cb_matrix_driver(name, config, envir = parent.frame())
  # Widen step 1 gender to {F, M} so the later narrowing to {F} is a real change.
  cb_set_discrete(app, "1", "gender", c("F", "M"))
  cb_add_step(app)
  app
}

# Narrow step 1 gender to {F} (the non-last-step filter update under test).
narrow_step1_to_F <- function(app) {
  cb_set_discrete(app, "1", "gender", c("F"))
}

# ── Choices/ranges react to a non-last-step update (propagate cache/data) ─────

test_that("Update non-last step: data-mode narrows downstream group + age (domain)", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-data-domain",
    list(run_button = "none", propagate = "data",
         render_source = "domain", cache = TRUE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  # Before the narrowing update, step 1 already allows gender {F, M}, so the
  # step was added against the parent's full-data domain: all groups {A, B, C}
  # are present and the age range spans the data (28-61), not the declared
  # {18, 80}. (Adding a step propagates from the resolved parent.)
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))
  expect_equal(cb_range_bounds(app, "2", "age"), c(28, 61))

  narrow_step1_to_F(app)

  # After the update the downstream domain narrows to the remaining data.
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("B", "C"))
  expect_equal(cb_range_bounds(app, "2", "age"), c(35, 50))
})

test_that("Update non-last step: cache-mode narrows downstream group (domain)", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-cache-domain",
    list(run_button = "none", propagate = "cache",
         render_source = "domain", cache = TRUE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))
  narrow_step1_to_F(app)
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("B", "C"))
})

test_that("Update non-last step: none-mode leaves downstream domain unchanged", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-none-domain",
    list(run_button = "none", propagate = "none",
         render_source = "domain", cache = TRUE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))
  narrow_step1_to_F(app)
  # No propagation: the declared downstream domain stays {A, B, C}.
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))
})

test_that("Update non-last step: filter-mode does not narrow from upstream data", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-filter-domain",
    list(run_button = "none", propagate = "filter",
         render_source = "domain", cache = TRUE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  narrow_step1_to_F(app)
  # filter mode narrows a filter from its own post-filter values, not from the
  # upstream gender restriction, so the group domain remains {A, B, C}.
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))
})

test_that("Update non-last step: cache=FALSE data-mode still narrows downstream", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-nocache-data-domain",
    list(run_button = "none", propagate = "data",
         render_source = "domain", cache = FALSE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  narrow_step1_to_F(app)
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("B", "C"))
})

# ── Pending state on a non-last-step update ──────────────────────────────────

test_that("Update non-last step: no run button -> nothing pending after update", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-none-pending",
    list(run_button = "none", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  narrow_step1_to_F(app)
  expect_false(cb_is_pending(app, "1"))
  expect_false(cb_is_pending(app, "2"))
})

test_that("Update non-last step: global run button -> updated step pending until run", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-global-pending",
    list(run_button = "global", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)
  # Clear the initial pending state from setup (run everything first).
  cb_run_all(app)
  expect_false(cb_is_pending(app, "1"))

  narrow_step1_to_F(app)
  # Editing a non-last step marks it pending again.
  expect_true(cb_is_pending(app, "1"))

  cb_run_all(app)
  expect_false(cb_is_pending(app, "1"))
  expect_false(cb_is_pending(app, "2"))
})

test_that("Update non-last step: local run button -> updated step pending until run", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-local-pending",
    list(run_button = "local", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)
  cb_run_step(app, "1")
  cb_run_step(app, "2")
  expect_false(cb_is_pending(app, "1"))

  narrow_step1_to_F(app)
  expect_true(cb_is_pending(app, "1"))

  cb_run_step(app, "1")
  expect_false(cb_is_pending(app, "1"))
})

# ── Statistics react to a non-last-step update ──────────────────────────────

test_that("Update non-last step: stats recompute downstream (pre+post)", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-stats-recompute",
    list(run_button = "none", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post")
  )
  on.exit(app$stop(), add = TRUE)

  stats_before <- cb_stats_html(app, "2")
  expect_true(!is.null(stats_before) && nchar(stats_before) > 0)

  narrow_step1_to_F(app)

  stats_after <- cb_stats_html(app, "2")
  expect_true(!is.null(stats_after) && nchar(stats_after) > 0)
  # Post stats are present and the downstream stats block changed (fewer rows).
  expect_match(stats_after, "cb_delayed", fixed = TRUE)
  expect_false(identical(stats_before, stats_after))
})

test_that("Update non-last step: stats=NULL keeps no stats after update", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-stats-none",
    list(run_button = "none", propagate = "data",
         render_source = "domain", cache = FALSE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)

  narrow_step1_to_F(app)
  stats <- cb_stats_html(app, "2")
  if (!is.null(stats)) {
    expect_false(grepl("cb_delayed", stats))
  }
})

# ── Feedback plots react to a non-last-step update ──────────────────────────

test_that("Update non-last step: feedback plots present downstream after update", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-feedback-on",
    list(run_button = "none", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post",
         feedback = TRUE)
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 10000)

  narrow_step1_to_F(app)
  app$wait_for_idle(timeout = 10000)

  expect_true(cb_has_feedback(app, "2", "gender"))
  expect_true(cb_has_feedback(app, "1", "gender"))
})

test_that("Update non-last step: feedback=FALSE shows no plots after update", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-feedback-off",
    list(run_button = "none", propagate = "data",
         render_source = "auto", cache = TRUE, stats = "pre+post",
         feedback = FALSE)
  )
  on.exit(app$stop(), add = TRUE)

  narrow_step1_to_F(app)
  expect_false(cb_has_feedback(app, "2", "gender"))
})

# ── run_button + domain narrowing interaction ───────────────────────────────

test_that("Update non-last step: global run defers downstream narrowing until run", {
  skip_on_cran(); skip_on_ci(); skip_if_screenshot_only()

  app <- setup_two_step(
    "upd-global-defer-narrow",
    list(run_button = "global", propagate = "data",
         render_source = "domain", cache = TRUE, stats = "none")
  )
  on.exit(app$stop(), add = TRUE)
  cb_run_all(app)
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("A", "B", "C"))

  narrow_step1_to_F(app)
  # Run button on: the edit is pending, downstream domain not yet recomputed.
  expect_true(cb_is_pending(app, "1"))

  cb_run_all(app)
  # After running, propagation narrows the downstream domain.
  expect_setequal(cb_discrete_choices(app, "2", "group"), c("B", "C"))
})
