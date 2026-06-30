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

# Launch the parametrized matrix app (tests/testthat/apps/matrix) under a given
# configuration. The configuration is passed to the spawned app process via
# environment variables (inherited by shinytest2's child process). Env vars are
# set just before launch and cleared on the caller's exit so they don't leak
# into other tests.
#
# `config` is a named list with any of: run_button, propagate, render_source,
# compute_stats, stats, feedback. Missing entries fall back to the app's own defaults.
cb_matrix_driver <- function(name, config = list(), envir = parent.frame(),
                             ready_timeout = 30000) {
  env_map <- c(
    run_button    = "SCB_RUN_BUTTON",
    propagate     = "SCB_PROPAGATE",
    render_source = "SCB_RENDER_SOURCE",
    compute_stats = "SCB_COMPUTE_STATS",
    stats         = "SCB_STATS",
    feedback      = "SCB_FEEDBACK"
  )

  to_set <- list()
  for (key in names(config)) {
    var <- env_map[[key]]
    if (is.null(var)) {
      stop("Unknown matrix config key: ", key)
    }
    value <- config[[key]]
    if (is.logical(value)) {
      value <- if (isTRUE(value)) "TRUE" else "FALSE"
    }
    to_set[[var]] <- as.character(value)
  }

  if (length(to_set) > 0) {
    do.call(Sys.setenv, to_set)
    # Clear on caller exit so configs do not leak across tests.
    withr::defer(Sys.unsetenv(names(to_set)), envir = envir)
  }

  cb_app_driver(test_path("apps", "matrix"), name = name,
                ready_timeout = ready_timeout)
}

# Toggle a discrete filter's checkbox group to exactly the supplied values.
# `step` and `filter` identify the input (e.g. step "1", filter "gender").
cb_set_discrete <- function(app, step, filter, values, ns = "coh",
                            wait = TRUE, timeout = 8000) {
  vals_js <- paste0(
    "[", paste(sprintf("'%s'", values), collapse = ","), "]"
  )
  input_id <- sprintf("%s-%s-%s-val", ns, step, filter)
  app$run_js(sprintf(
    "var want = %s;
     var cbs = $('#%s input[type=checkbox]');
     cbs.each(function() { this.checked = want.indexOf($(this).val()) !== -1; });
     $('#%s.shiny-input-checkboxgroup').trigger('change');",
    vals_js, input_id, input_id
  ))
  if (wait) app$wait_for_idle(timeout = timeout)
  invisible(app)
}

# Click the "add step" (clone) control and wait for the new step to render.
cb_add_step <- function(app, ns = "coh", timeout = 12000) {
  app$click(selector = sprintf("#%s-cb_panel .cb_add_step", ns))
  app$wait_for_idle(timeout = timeout)
  invisible(app)
}

# Click a step's local run button (run_button = "local") and wait.
cb_run_step <- function(app, step, ns = "coh", timeout = 12000) {
  app$click(selector = sprintf("#%s-%s .cb_run_step", ns, step))
  app$wait_for_idle(timeout = timeout)
  invisible(app)
}

# Click the global run button (run_button = "global") and wait.
cb_run_all <- function(app, ns = "coh", timeout = 15000) {
  app$click(selector = sprintf("#%s-cb_panel .cb_trigger_run", ns))
  app$wait_for_idle(timeout = timeout)
  invisible(app)
}

# Read the rendered discrete choice values for a step/filter checkbox group,
# e.g. the available group letters {A,B,C} in step 2. Returns a sorted character
# vector (empty when the input is not present).
cb_discrete_choices <- function(app, step, filter, ns = "coh") {
  html <- tryCatch(
    app$get_html(sprintf("#%s-%s-%s-val", ns, step, filter)),
    error = function(e) NULL
  )
  if (is.null(html) || !nzchar(html)) return(character())
  vals <- regmatches(html, gregexpr("value=\"([^\"]*)\"", html))[[1]]
  vals <- sub("^value=\"", "", sub("\"$", "", vals))
  sort(unique(vals))
}

# Read the live numeric range bounds (min/max) for a step/filter slider input.
#
# The slider is an ionRangeSlider widget; updateSliderInput re-bounds the live
# widget but does NOT rewrite the static data-min/data-max HTML attributes, so
# we read the widget's own options via JS to observe the current (possibly
# propagated/narrowed) bounds. Returns numeric c(min, max) or NULL.
cb_range_bounds <- function(app, step, filter, ns = "coh") {
  js <- sprintf(
    "(function(){var s=$('#%s-%s-%s-slider').data('ionRangeSlider');
       if(!s) return null;
       return JSON.stringify({min:s.options.min, max:s.options.max});})()",
    ns, step, filter
  )
  out <- tryCatch(app$get_js(js), error = function(e) NULL)
  if (is.null(out) || identical(out, "null")) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(out), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  c(as.numeric(parsed$min), as.numeric(parsed$max))
}

# Does a step/filter show a feedback plot? (.cb_feedback within the filter)
cb_has_feedback <- function(app, step, filter, ns = "coh") {
  html <- tryCatch(
    app$get_html(sprintf("#%s-%s .cb_filter[data-filter_id='%s']", ns, step, filter)),
    error = function(e) NULL
  )
  if (is.null(html)) return(FALSE)
  grepl("cb_feedback", html, fixed = TRUE)
}

# Read the per-step stats HTML for the patients dataset (or NULL/"" if absent).
cb_stats_html <- function(app, step, ns = "coh", dataset = "patients") {
  tryCatch(
    app$get_html(sprintf("#%s-%s-stats_%s", ns, step, dataset)),
    error = function(e) NULL
  )
}

# Is a step currently marked pending? (pending class on #<ns>-<step>)
cb_is_pending <- function(app, step, ns = "coh") {
  step_html <- tryCatch(
    app$get_html(sprintf("#%s-%s", ns, step)),
    error = function(e) ""
  )
  if (is.null(step_html)) return(FALSE)
  # The pending class lives on the step root element; match it on the class attr
  # of the step container to avoid false positives from descendant text.
  grepl("class=\"[^\"]*\\bpending\\b", step_html) ||
    grepl("class='[^']*\\bpending\\b", step_html)
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
