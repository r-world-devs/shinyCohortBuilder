# should_run_step_on_render() decides whether a step is (re)computed while the
# panel is rendered. It is the bridge that keeps a restored state consistent:
# after restore() (always called with run_flow = FALSE in the app), the data
# slots are wiped, but each step still carries its restored pending flag. This
# function ensures resolved (non-pending) steps are recomputed on render so the
# displayed data matches the restored state, while pending steps in run_button
# mode are left for an explicit Run.

mk_two_step <- function(run = FALSE) {
  coh <- cohortBuilder::cohort(
    cohortBuilder::set_source(cohortBuilder::tblist(iris = iris)),
    cohortBuilder::step(cohortBuilder::filter(
      "discrete", id = "sp", dataset = "iris", variable = "Species",
      value = c("setosa", "versicolor"), active = TRUE
    )),
    cohortBuilder::step(cohortBuilder::filter(
      "range", id = "sl", dataset = "iris", variable = "Sepal.Length",
      range = c(5, 7), active = TRUE
    ))
  )
  if (run) cohortBuilder::run(coh)
  coh
}

test_that("init render: auto-run mode runs pending steps, skips resolved", {
  coh <- mk_two_step()
  # init = TRUE, not run_button: render runs steps that are still pending.
  expect_true(should_run_step_on_render(coh, "1", init = TRUE, run_on_request = FALSE))
  cohortBuilder::run(coh)
  expect_false(should_run_step_on_render(coh, "1", init = TRUE, run_on_request = FALSE))
})

test_that("init render: run_button mode never runs on init", {
  coh <- mk_two_step()
  expect_false(should_run_step_on_render(coh, "1", init = TRUE, run_on_request = TRUE))
  expect_false(should_run_step_on_render(coh, "2", init = TRUE, run_on_request = TRUE))
})

test_that("restore render: auto-run mode always recomputes every step", {
  # init = FALSE (restore / update_source rebuild). In auto-run mode every step
  # is recomputed, so wiped data is rebuilt and stays consistent.
  coh <- mk_two_step()
  expect_true(should_run_step_on_render(coh, "1", init = FALSE, run_on_request = FALSE))
  expect_true(should_run_step_on_render(coh, "2", init = FALSE, run_on_request = FALSE))
})

test_that("restore render: run_button mode runs step 1 and resolved steps", {
  # Step 1's input is the source (always available) so it always renders.
  # Steps 2+ are recomputed only when restored as resolved (pending = FALSE).
  resolved <- mk_two_step(run = TRUE)
  expect_true(should_run_step_on_render(resolved, "1", init = FALSE, run_on_request = TRUE))
  expect_true(should_run_step_on_render(resolved, "2", init = FALSE, run_on_request = TRUE))
})

test_that("restore render: run_button mode leaves pending steps 2+ uncomputed", {
  pending <- mk_two_step() # both steps pending (never run)
  # Step 1 still renders (source input), step 2 waits for an explicit Run.
  expect_true(should_run_step_on_render(pending, "1", init = FALSE, run_on_request = TRUE))
  expect_false(should_run_step_on_render(pending, "2", init = FALSE, run_on_request = TRUE))
})

test_that("restore render: run_button mixed pending state is respected", {
  coh <- mk_two_step(run = TRUE)
  coh$set_pending("2", pending = TRUE) # step 1 resolved, step 2 pending
  expect_true(should_run_step_on_render(coh, "1", init = FALSE, run_on_request = TRUE))
  expect_false(should_run_step_on_render(coh, "2", init = FALSE, run_on_request = TRUE))
})

test_that("restored resolved state renders consistent data (run_button mode)", {
  # End-to-end: restore a resolved state (run_flow = FALSE wipes data), then
  # emulate render by running the steps should_run_step_on_render() selects.
  # The resulting data must match the originally-saved (filtered) data.
  ref <- mk_two_step(run = TRUE)
  exp_s1 <- nrow(ref$get_data("1", state = "post")$iris)
  exp_s2 <- nrow(ref$get_data("2", state = "post")$iris)

  fresh <- mk_two_step()
  fresh$restore(ref$get_state(json = FALSE), run_flow = FALSE)
  for (sid in names(fresh$get_step())) {
    if (should_run_step_on_render(fresh, sid, init = FALSE, run_on_request = TRUE)) {
      fresh$run_step(sid)
    }
  }

  expect_identical(nrow(fresh$get_data("1", state = "post")$iris), exp_s1)
  expect_identical(nrow(fresh$get_data("2", state = "post")$iris), exp_s2)
})
