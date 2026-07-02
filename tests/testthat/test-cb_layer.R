# Build a cohort with a session whose sendCustomMessage records the message
# types it receives, so hook side effects can be asserted.
build_hook_cohort <- function(run_button) {
  source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = iris))
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris",
      variable = "Species", value = "setosa"
    )
  )
  coh$attributes$run_button <- run_button
  sent <- new.env()
  sent$types <- character(0)
  coh$attributes$session <- list(
    ns = function(x) x,
    sendCustomMessage = function(type, message) {
      sent$types <- c(sent$types, type)
      invisible(NULL)
    }
  )
  list(coh = coh, sent = sent)
}

# Run post_rm_step_hook with the UI/data side effects mocked out, returning the
# custom message types that were sent.
run_rm_hook <- function(run_button) {
  fixture <- build_hook_cohort(run_button)
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      post_rm_step_hook(
        fixture$coh, fixture$coh$.__enclos_env__$private, "1"
      ),
      clear_step_data = function(...) invisible(NULL)
    ),
    removeUI = function(...) invisible(NULL),
    .package = "shiny"
  )
  fixture$sent$types
}

test_that("post_rm_step_hook informs data update in auto-run mode", {
  msgs <- run_rm_hook("none")
  # UI cleanup and the data-updated signal both fire when steps run eagerly.
  expect_true("post_rm_step_action" %in% msgs)
  expect_true("inform_data_updated" %in% msgs)
})

test_that("post_rm_step_hook does not inform data update in run_button mode", {
  # In run_button mode data is computed only on an explicit Run, so removing a
  # step must not push stale/uncomputed data to the returned-data reactive. The
  # UI cleanup still runs.
  for (mode in c("global", "local")) {
    msgs <- run_rm_hook(mode)
    expect_true(
      "post_rm_step_action" %in% msgs,
      info = sprintf("run_button = %s", mode)
    )
    expect_false(
      "inform_data_updated" %in% msgs,
      info = sprintf("run_button = %s", mode)
    )
  }
})
