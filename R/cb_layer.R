attach_filter_gui <- function(filter) {
  filter$gui <- .gui_filter(filter)
  return(filter)
}

attach_filters_gui <- function(step) {
  filter_names <- names(step$filters)
  step$filters <- step$filters %>%
    purrr::modify(attach_filter_gui)
  return(step)
}

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
      gui_rm_step(public, list(step_id = step_id), session)
    }
    return(invisible(TRUE))
  }
  if (identical(keep_steps, TRUE)) {
    keep_steps <- as.integer(names(public$get_step()))
  }
  if (is.integer(keep_steps)) {
    open_step <- length(keep_steps)
    for (step_id in as.character(setdiff(n_steps:1, keep_steps))) {
      gui_rm_step(public, list(step_id = step_id), session)
    }
  }
  session$sendCustomMessage("enroll_accordion", list(id = session$ns("cb_steps"), index = open_step - 1))
}

post_update_source_hook <- function(public, private, keep_steps, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  reset <- TRUE
  if (identical(keep_steps, TRUE)) {
    reset <- FALSE
  }
  if (is.integer(keep_steps) || isTRUE(keep_steps)) {
    for (step_id in names(public$get_step())) {
      if (reset) public$clear_step(step_id)
      .trigger_action(session, "clear_step", params = list(step_id = step_id, reset = reset))
    }
  }
  if (identical(keep_steps, FALSE)) {
    render_steps(public, session, init = FALSE)
  }
}

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
    gui_rm_step(public, list(step_id = step_id), session)
  }

  return(invisible(TRUE))
}

post_restore_hook <- function(public, private, ...) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  render_steps(public, session, init = FALSE)
}

post_run_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  if (step_id == public$last_step_id()) {
    session$sendCustomMessage(
      "inform_data_updated",
      list(steps = public$last_step_id(), ns_prefix = session$ns(""))
    )
  }
}

post_rm_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  session$sendCustomMessage(
    "inform_data_updated",
    list(steps = `%:::%`("cohortBuilder", "prev_step")(step_id), ns_prefix = session$ns(""))
  )
}

enable_panel <- function(cohort, session) {
  if (cohort$last_step_id() != "0") {
    session$sendCustomMessage("enable_panel", list(enable = TRUE, ns_prefix = session$ns("")))
  }
}

post_cohort_hook <- function(public, private, ...) {
  source <- public$get_source()
  if (!is.null(source)) {
    available_filters <- source$get("available_filters")
    if (!is.null(available_filters)) {
      public$attributes$available_filters <- purrr::map(available_filters, ~ .x(source))
    }
  }
}


.onLoad <- function(libname, pkgname){

  cohortBuilder::add_hook("pre_update_source_hook", pre_update_source_hook)
  cohortBuilder::add_hook("post_update_source_hook", post_update_source_hook)
  cohortBuilder::add_hook("post_run_step_hook", post_run_step_hook)
  cohortBuilder::add_hook("post_rm_step_hook", post_rm_step_hook)
  cohortBuilder::add_hook("pre_restore_hook", pre_restore_hook)
  cohortBuilder::add_hook("post_restore_hook", post_restore_hook)
  cohortBuilder::add_hook("post_cohort_hook", post_cohort_hook)
  cohortBuilder::add_hook("post_update_source_hook", post_cohort_hook)

}
