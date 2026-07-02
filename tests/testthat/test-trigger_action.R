test_that(".trigger_action_js generates valid JS with default namespace", {
  js <- .trigger_action_js("update_filter")
  expect_type(js, "character")
  expect_true(grepl("Shiny.setInputValue", js))
  expect_true(grepl("update_filter", js))
  expect_true(grepl("priority", js))
})

test_that(".trigger_action_js applies namespace function", {
  ns <- shiny::NS("mymod")
  js <- .trigger_action_js("add_step", ns = ns)
  expect_true(grepl("mymod-action", js))
  expect_true(grepl("add_step", js))
})

test_that(".trigger_action_js includes params in JSON", {
  js <- .trigger_action_js(
    "rm_step",
    params = list(step_id = "2")
  )
  expect_true(grepl("rm_step", js))
  expect_true(grepl("step_id", js))
  expect_true(grepl("2", js))
})

test_that(".trigger_action_js with empty params works", {
  js <- .trigger_action_js("show_state", params = list())
  expect_true(grepl("show_state", js))
})

test_that(".trigger_action_js generates correct action IDs", {
  actions <- c(
    "update_filter", "add_step", "rm_step", "clear_step",
    "update_data_stats", "show_repro_code",
    "run_step", "show_state", "input_state", "restore_state",
    "show_attrition", "show_help", "manage_step_modal"
  )
  for (action in actions) {
    js <- .trigger_action_js(action)
    expect_true(grepl(action, js), info = paste("Action:", action))
  }
})

test_that(".trigger_action_js with nested params serializes correctly", {
  js <- .trigger_action_js(
    "update_filter",
    params = list(
      step_id = "1",
      filter_id = "age",
      input_name = "range",
      run_flow = TRUE
    )
  )
  expect_true(grepl("step_id", js))
  expect_true(grepl("filter_id", js))
  expect_true(grepl("run_flow", js))
})
