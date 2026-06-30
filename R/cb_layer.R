# cohortBuilder lifecycle hooks
#
# The functions below are registered with cohortBuilder via `add_hook()` in
# `.onLoad()` and form the bridge between the R6 cohort's lifecycle events and
# the Shiny UI. They are invoked by cohortBuilder with the cohort's `public`
# (the R6 public interface) and `private` environments plus event-specific
# arguments, and they early-return when no Shiny `session` is attached (i.e. the
# cohort is used programmatically rather than through `cb_server()`). See the
# Hooks section of the package architecture docs for the high-level flow.

#' Attach the GUI layer to a single filter (idempotent)
#'
#' Lazily constructs the filter's `.gui_filter()` object and stores it on
#' `filter@private$gui`; a no-op when one is already attached.
#'
#' @param filter A cohortBuilder filter object.
#' @return The filter, with its GUI object attached.
#' @noRd
attach_filter_gui <- function(filter) {
  if (!is.null(filter@private$gui)) {
    return(filter)
  }
  filter@private$gui <- rlang::exec(.gui_filter, filter, !!!filter@extra$gui_args)
  return(filter)
}

#' Attach the GUI layer to every filter of a step
#' @param step A cohortBuilder step holding `$filters`.
#' @return The step, with GUI objects attached to all its filters.
#' @noRd
attach_filters_gui <- function(step) {
  filter_names <- names(step$filters)
  step$filters <- step$filters |>
    purrr::modify(attach_filter_gui)
  return(step)
}

#' Hook: tear down step UI before the source is updated
#'
#' Removes the step UI that will not be kept when the source changes, and
#' re-opens the appropriate accordion step.
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param keep_steps Which steps to keep: `FALSE` (none), `TRUE` (all), or an
#'   integer vector of step ids.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `TRUE`/`FALSE`; called for its UI side effects.
#' @noRd
pre_update_source_hook <- function(public, private, keep_steps, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  n_steps <- as.integer(public$last_step_id())
  if (n_steps == 0) {
    return(invisible(TRUE))
  }
  # todo remove cache from first step
  open_step <- 1
  if (identical(keep_steps, FALSE)) {
    for (step_id in as.character(n_steps:1)) {
      action_rm_step(public, list(step_id = step_id), session)
    }
    return(invisible(TRUE))
  }
  if (identical(keep_steps, TRUE)) {
    keep_steps <- as.integer(names(public$get_step()))
  }
  if (is.integer(keep_steps)) {
    open_step <- length(keep_steps)
    for (step_id in as.character(setdiff(n_steps:1, keep_steps))) {
      action_rm_step(public, list(step_id = step_id), session)
    }
  }
  session$sendCustomMessage("enroll_accordion", list(id = session$ns("cb_steps"), index = open_step - 1))
}

#' Hook: re-render / reset step UI after the source is updated
#'
#' Re-attaches the GUI to kept steps and either clears or re-renders them
#' depending on `keep_steps`.
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param keep_steps Which steps to keep: `FALSE`, `TRUE`, or an integer vector.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_update_source_hook <- function(public, private, keep_steps, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  reset <- TRUE
  if (identical(keep_steps, TRUE)) {
    reset <- FALSE
    for (step_id in names(public$get_step())) {
      private$steps[[step_id]] <- attach_filters_gui(private$steps[[step_id]])
    }
  }
  if (is.integer(keep_steps) || isTRUE(keep_steps)) {
    for (step_id in names(public$get_step())) {
      if (reset) {
        public$clear_step(step_id)
      }
      .trigger_action(session, "clear_step", params = list(step_id = step_id, reset = reset))
    }
  }
  if (identical(keep_steps, FALSE)) {
    render_steps(public, session, init = FALSE)
  }
}

#' Hook: remove all step UI before restoring cohort state
#' @param public,private The cohort's R6 public/private environments.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `TRUE`/`FALSE`; called for its UI side effects.
#' @noRd
pre_restore_hook <- function(public, private, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  n_steps <- as.integer(public$last_step_id())
  if (n_steps == 0) {
    return(invisible(TRUE))
  }
  for (step_id in as.character(n_steps:1)) {
    action_rm_step(public, list(step_id = step_id), session)
  }

  return(invisible(TRUE))
}

#' Hook: re-render all step UI after restoring cohort state
#' @param public,private The cohort's R6 public/private environments.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_restore_hook <- function(public, private, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  render_steps(public, session, init = FALSE)
}

#' Hook: refresh inputs, stats and feedback after a step runs
#'
#' Updates each filter's input and plot, refreshes data statistics, and signals
#' the data-updated input(s) so dependent reactives recompute.
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the step that ran.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_run_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  ui_update_filters_loop(
    public, step_id, reset = FALSE,
    update = c("input", "plot"), session = session
  )
  action_update_data_stats(public, list(step_id = step_id), session)

  session$sendCustomMessage(
    "inform_data_updated",
    list(step_id = step_id, ns_prefix = session$ns(""))
  )

  if (step_id == public$last_step_id()) {
    session$sendCustomMessage(
      "inform_data_updated",
      list(steps = public$last_step_id(), ns_prefix = session$ns(""))
    )
  }
}

#' Hook: remove step UI and signal downstream after a step is removed
#'
#' Clears the step's cached data and UI; signals a data update for the previous
#' step unless the cohort runs on explicit request (run-button mode), where
#' pushing results before a Run would expose stale data.
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the removed step.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_rm_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  clear_step_data(step_id, session)
  shiny::removeUI(
    glue::glue("#{session$ns(step_id)}"),
    session = session, immediate = TRUE
  )
  session$sendCustomMessage(
    "post_rm_step_action",
    list(id = step_id, ns_prefix = session$ns(""))
  )
  # In run_button mode data is computed only on an explicit Run. Signalling a
  # data update on step removal would push stale/uncomputed results to the
  # returned-data reactive before the user runs the flow, so skip it. The UI
  # cleanup above (removeUI / post_rm_step_action) still runs.
  run_on_request <- !is_none(public$attributes$run_button)
  if (!run_on_request) {
    session$sendCustomMessage(
      "inform_data_updated",
      list(steps = `%:::%`("cohortBuilder", "prev_step")(step_id), ns_prefix = session$ns(""))
    )
  }
}

#' Hook: render the UI for a newly added step
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the added step.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_add_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  session$sendCustomMessage("pre_add_step_action", list(id = step_id, ns_prefix = session$ns("")))
  render_step(
    public,
    step_id,
    active = TRUE,
    allow_rm = TRUE,
    session$input, session$output, session
  )
}

#' Hook: refresh inputs/feedback and cascade after a filter value changes
#'
#' In immediate mode runs the step (and cascades to downstream steps); refreshes
#' the changed filter's input and plot, sibling post-stats, and toggles
#' active/inactive styling. In run-button mode UI is refreshed but no run is
#' triggered.
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id,filter_id Ids of the changed step and filter.
#' @param active Optional new active state for the filter (when toggling on/off).
#' @param hook_args List of extra hook options, e.g. `update` (which UI pieces to refresh).
#' @param ... Unused extra hook arguments.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_update_filter_hook <- function(public, private, step_id, filter_id, ..., active,
                                    hook_args = list(update = "input")) {

  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  update_active <- !missing(active)

  run_on_request <- !is_none(public$attributes$run_button)
  if (!run_on_request) {
    public$run_step(step_id)
  }

  force_render <- getOption("scb_render_all", default = FALSE)
  run_update <- TRUE
  if (!force_render && update_active) {
    run_update <- !ui_insert_filter_content(step_id, filter_id, public, session)
  }

  data_filter <- public$get_filter(step_id, filter_id)
  filter_stats <- if_null_default(
    data_filter@extra$stats,
    public$attributes$stats
  )
  update <- hook_args$update
  post_stats_visible <- "post" %in% filter_stats
  if (run_update) {
    update <- c(update, "plot", "multi_input")
    if (!run_on_request && post_stats_visible) {
      update <- c(update, "post_input")
    }
    ui_update_filter(public, step_id, filter_id, update, FALSE, session)
  }

  if (!run_on_request && ("post" %in% public$attributes$stats)) {
    update <- "post_input"
    ui_update_filters_loop(public, step_id, FALSE, update, exclude = filter_id, session)
  }

  if (isTRUE(update_active)) {
    ui_update_filter_class(step_id, filter_id, active, "hidden-input", session)
    session$sendCustomMessage(
      "update_filter_active",
      list(
        step_id = step_id, filter_id = filter_id,
        active = active, ns_prefix = session$ns("")
      )
    )
  }

  # Cascade to subsequent steps
  if (!run_on_request) {
    next_step_id <- as.integer(step_id) + 1
    if (next_step_id <= length(public$get_step())) {
      public$run_flow(min_step = as.character(next_step_id))
    }
  }
}

#' Enable the filtering panel once at least one step exists
#' @param cohort The cohort object.
#' @param session Shiny session.
#' @return Invisibly `NULL`; sends the `enable_panel` custom message.
#' @noRd
enable_panel <- function(cohort, session) {
  if (cohort$last_step_id() != "0") {
    session$sendCustomMessage("enable_panel", list(enable = TRUE, ns_prefix = session$ns("")))
  }
}

#' Hook: cache the source's available filters on the cohort attributes
#' @param public,private The cohort's R6 public/private environments.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `NULL`; called for its side effect.
#' @noRd
post_cohort_hook <- function(public, private, ...) {
  source <- public$get_source()
  if (!is.null(source)) {
    available_filters <- source$available_filters
    if (!is.null(available_filters)) {
      public$attributes$available_filters <- available_filters
    }
  }
}

#' Hook: attach the GUI to all filters after the source initialises
#' @param public,private The cohort's R6 public/private environments.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `NULL`; called for its side effect.
#' @noRd
post_init_source_hook <- function(public, private, ...) {
  for (a_step in private$steps) {
    public$modify(function(public, private) {
      step_id <- a_step$id
      private$steps[[step_id]] <- attach_filters_gui(private$steps[[step_id]])
    })
  }
}

#' Hook: reflect a step's pending (needs-run) state in the UI
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the step whose pending state changed.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_set_pending_hook <- function(public, private, step_id, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  ui_update_pending_state(session, public, step_id)
}

#' Hook: refresh rendered inputs after domains propagate (domain mode only)
#'
#' Only acts when `render_source = "domain"` and the step's filters already have
#' a GUI attached; in stats mode the stats-driven refreshes cover downstream
#' changes, and unrendered steps are rendered fresh by [post_add_step_hook()].
#'
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the step whose domain changed.
#' @param ... Unused extra hook arguments.
#' @return Invisibly `FALSE` when skipped; otherwise called for side effects.
#' @noRd
post_propagate_domains_hook <- function(public, private, step_id, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  # Only refresh rendered inputs when the app is actually rendering from domains.
  # In stats mode the stats-driven refresh (post_run_step_hook / update_filter
  # hook) already covers downstream changes and must not be duplicated.
  render_source <- public$attributes$render_source
  if (is.null(render_source) || !identical(render_source, "domain")) {
    return(invisible(FALSE))
  }

  # Skip steps that are not yet rendered. When a step is added, its domain is
  # propagated eagerly (before render), but its filters have no GUI attached yet
  # and there are no inputs to update. The step is rendered fresh with the
  # already-narrowed domain by post_add_step_hook, so refreshing here is both
  # unnecessary and unsafe (filter@private$gui$update is NULL).
  step <- public$get_step(step_id)
  if (is.null(step) || !step_gui_attached(step)) {
    return(invisible(FALSE))
  }

  ui_update_filters_loop(
    public, step_id, reset = FALSE,
    update = "input", session = session
  )
}

#' Has a step's GUI been attached to all its filters?
#'
#' A step's filters get their GUI attached during render
#' ([attach_filters_gui()]); until then there are no rendered inputs to refresh.
#'
#' @param step A cohortBuilder step.
#' @return `TRUE` when the step has filters and all carry a GUI object.
#' @noRd
step_gui_attached <- function(step) {
  if (length(step$filters) == 0L) {
    return(FALSE)
  }
  all(purrr::map_lgl(step$filters, ~ !is.null(.x@private$gui)))
}

#' Hook: insert UI for a filter added to a step
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id Id of the step the filter was added to.
#' @param filter The added cohortBuilder filter object.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_add_filter_hook <- function(public, private, step_id, filter) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  ui_insert_filter(step_id = step_id, filter_id = filter@id, cohort = public, session = session)
}

#' Hook: remove UI for a filter dropped from a step
#' @param public,private The cohort's R6 public/private environments.
#' @param step_id,filter_id Ids of the affected step and removed filter.
#' @return Invisibly `FALSE` when no session; otherwise called for side effects.
#' @noRd
post_rm_filter_hook <- function(public, private, step_id, filter_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  ui_remove_filter(step_id = step_id, filter_id = filter_id, cohort = public, session = session)
}

#' Register cohortBuilder hooks on package load
#' @param libname,pkgname Standard `.onLoad` arguments (unused).
#' @return Invisibly `NULL`; registers hooks as a side effect.
#' @noRd
.onLoad <- function(libname, pkgname){
  cohortBuilder::add_hook("pre_update_source_hook", pre_update_source_hook)
  cohortBuilder::add_hook("post_update_source_hook", post_update_source_hook)
  cohortBuilder::add_hook("post_run_step_hook", post_run_step_hook)
  cohortBuilder::add_hook("post_rm_step_hook", post_rm_step_hook)
  cohortBuilder::add_hook("pre_restore_hook", pre_restore_hook)
  cohortBuilder::add_hook("post_restore_hook", post_restore_hook)
  cohortBuilder::add_hook("post_add_step_hook", post_add_step_hook)
  cohortBuilder::add_hook("post_update_filter_hook", post_update_filter_hook)
  cohortBuilder::add_hook("post_cohort_hook", post_cohort_hook)
  cohortBuilder::add_hook("post_update_source_hook", post_cohort_hook)
  cohortBuilder::add_hook("post_init_source_hook", post_init_source_hook)
  cohortBuilder::add_hook("post_set_pending_hook", post_set_pending_hook)
  cohortBuilder::add_hook("post_add_filter_hook", post_add_filter_hook)
  cohortBuilder::add_hook("post_rm_filter_hook", post_rm_filter_hook)
  cohortBuilder::add_hook("post_propagate_domains_hook", post_propagate_domains_hook)
}

#' Clear registered cohortBuilder hooks on package unload
#' @param libpath Standard `.onUnload` argument (unused).
#' @return Invisibly `NULL`; clears hook options as a side effect.
#' @noRd
.onUnload <- function(libpath) {
  options("pre_update_source_hook" = NULL)
  options("post_update_source_hook" = NULL)
  options("post_run_step_hook" = NULL)
  options("post_rm_step_hook" = NULL)
  options("pre_restore_hook" = NULL)
  options("post_restore_hook" = NULL)
  options("post_add_step_hook" = NULL)
  options("post_update_filter_hook" = NULL)
  options("post_cohort_hook" = NULL)
  options("post_update_source_hook" = NULL)
  options("post_init_source_hook" = NULL)
  options("post_set_pending_hook" = NULL)
  options("post_add_filter_hook" = NULL)
  options("post_rm_filter_hook" = NULL)
}
