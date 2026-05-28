attach_filter_gui <- function(filter) {
  if (!is.null(filter$gui)) {
    return(filter)
  }
  filter$gui <- rlang::exec(.gui_filter, filter, !!!filter$get_params("gui_args"))
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

post_add_step_hook <- function(public, private, step_id) {
  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  session$sendCustomMessage("pre_add_step_action", list(id = step_id, ns_prefix = session$ns("")))
  print("post_add_step_hook")
  print(step_id)
  #print(sum_up(public))
  render_step(
    public,
    step_id,
    active = TRUE,
    allow_rm = TRUE,
    session$input, session$output, session
  )
}

post_update_filter_hook <- function(public, private, step_id, filter_id, ..., active,
                                    hook_args = list(update_active = FALSE, update = NULL)) {

  session <- public$attributes$session
  if (is.null(session)) {
    return(invisible(FALSE))
  }
  print("post_update_filter_hook")
  print(step_id)
  print(filter_id)
  if (missing(active)) {
    active <- NULL
  }

  run_on_request <- !is_none(public$attributes$run_button)
  if (!run_on_request) {
    public$run_step(step_id)
  }

  force_render <- getOption("scb_render_all", default = FALSE)
  run_update <- TRUE
  if (!force_render && !is.null(active)) {
    run_update <- !insert_filter(step_id, filter_id, public, session)
  }

  data_filter <- public$get_filter(step_id, filter_id)
  filter_stats <- if_null_default(
    data_filter$get_params("stats"),
    public$attributes$stats
  )
  update <- hook_args$update
  post_stats_visible <- "post" %in% filter_stats
  if (run_update) {
    update <- c(update, "plot", "multi_input")
    if (!run_on_request && post_stats_visible) {
      update <- c(update, "post_input")
    }
    update_filter_gui(public, step_id, filter_id, update, FALSE, session)
  }

  print("b1")
  if (!run_on_request && ("post" %in% public$attributes$stats)) {
    update <- "post_input"
    gui_update_filters_loop(public, step_id, FALSE, update, exclude = filter_id, session)
  }
  print("b2")
  if (hook_args$update_active) {
    gui_update_filter_class(step_id, filter_id, active, "hidden-input", session)
  }
  print("b3")
  if (is_none(public$attributes$run_button)) {
    gui_update_data_stats(public, list(step_id = step_id), session)
    print("b4")
    update_next_step(public, step_id, FALSE, session)
  }
  print("done")
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

post_init_source_hook <- function(public, private, ...) {
  for (a_step in private$steps) {
    public$modify(function(public, private) {
      step_id <- a_step$id
      private$steps[[step_id]] <- attach_filters_gui(private$steps[[step_id]])
    })
  }
}


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
}

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
}
