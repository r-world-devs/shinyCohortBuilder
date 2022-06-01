call_filter <- function(filter_id, step_id, cohort, input, output, session) {
  filter <- cohort$get_filter(step_id, filter_id)
  no_data <- cohort$get_cache(step_id, filter_id, state = "pre")$n_data == 0
  if (cohort$attributes$feedback) {
    feedback <- filter$gui$feedback(sf_id(step_id, filter_id), cohort, no_data)
  }

  filter$gui$server(sf_id(step_id, filter_id), input, output, session, cohort)

  if (cohort$attributes$feedback) {
    session$output[[feedback$plot_id]] <- feedback$render_fun
  }
}

call_filters <- function(step_id, cohort, input, output, session) {
  cohort$get_filter(step_id, method = names) %>%
    purrr::walk(~call_filter(.x, step_id, cohort, input, output, session))
}

#' Create input controller insensitive to server updated
#'
#' @description
#' Input controllers created with `.cb_input` are sending its value to server only when
#' user changes it's value directly in browser.
#' That means all the `update*` functions have only visible effect on application output.
#'
#' The method should be used for each filter input controller and precise which
#' filter value should be updated when the input selection is changes.
#'
#' @param ui UI defining input controllers.
#' @param data_param Name of the parameter that should be updated in filter whenever user change the input value.
#' @param ... Extra attributes passed to the input div container.
#' @param priority Set to 'event' to force sending value.
#'
#' @export
.cb_input <- function(ui, data_param, ..., priority = NULL) {
  shiny::div(class = "cb_input", `data-param` = data_param, `data-exec_state` = "init", ui, priority = priority, ...)
}

render_filter_content <- function(step_filter_id, filter, cohort, ns) {
  feedback <- filter$gui$feedback(step_filter_id, cohort, NULL)
  cohort$attributes$session$userData$rendered_filters <- c(
    cohort$attributes$session$userData$rendered_filters,
    ns(step_filter_id)
  )

  shiny::tagList(
    if (cohort$attributes$feedback) {
      shiny::div(
        class = "cb_feedback",
        feedback$output_fun(ns(feedback$plot_id), height = "auto", width = "100%")
      )
    },
    shiny::div(
      class = "cb_inputs",
      filter$gui$input(ns(step_filter_id), cohort)
    )
  )
}

#' Define filter related output in filtering panel
#'
#' The method exported only for custom extensions use.
#'
#' @param filter Filter object.
#' @param step_id Id of the step.
#' @param cohort Cohort object.
#' @param ns Namespace function.
#'
#' @export
.render_filter <- function(filter, step_id, cohort, ns) {
  filter_id <- filter$id
  step_filter_id <- sf_id(step_id, filter_id)

  keep_na_filter <- filter$get_params("keep_na")
  input_param_name <- filter$input_param
  active_filter <- filter$get_params("active")
  filter_description <- cohort$show_help(step_id = step_id, filter_id = filter$id)
  no_data_class <- if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    "cb_no_data"
  } else {
    ""
  }
  force_render <- getOption("scb_render_all", default = FALSE)

  active_id <- paste0("active_", step_filter_id)
  shiny::div(
    class = "cb_filter ",
    id = ns(step_filter_id),
    `data-filter_id` = filter_id,
    filter_help_icon(filter, ns, "filter", filter_description, cohort),
    .cb_input(
      shinyWidgets::materialSwitch(
        inputId = ns(active_id), label = filter$name,
        value = active_filter, right = TRUE
      ),
      "active"
    ),
    shiny::div(
      class = paste(
        "cb_filter_content",
        if (active_filter) NULL else "hidden-input",
        no_data_class
      ),
      if (active_filter || force_render) {
        render_filter_content(step_filter_id, filter, cohort, ns)
      }
    )
  )
}

render_step <- function(cohort, step_id, active, allow_rm, input, output, session) {
  ns <- session$ns
  run_button <- cohort$attributes$run_button
  cohort$modify(function(public, private) {
    private$steps[[step_id]] <- attach_filters_gui(private$steps[[step_id]])
  })

  step <- cohort$get_step(step_id)

  shinyGizmo::addAccordionItem(
    accordionId = ns("cb_steps"),
    accordionItem = shinyGizmo::accordionItem(
      id = ns(step_id),
      `data-step_id` = step_id,
      class = "cb_step panel panel-default card",
      header_class = "panel-heading card-header",
      header = shiny::tagList(
        shiny::tags$strong(class = "cb_step_name", glue::glue("Step {step_id}")),
        shiny::tags$div(style = "float: right;",
          button(
            title = "Delete Step", class = "cb_rm_step",
            onclick = shiny::HTML(
              shinyGizmo::accordionEnrollOnClick(prev = TRUE),
              .trigger_action_js("rm_step", list(step_id = step_id), ns = ns)
            ),
            icon = shiny::icon("trash-alt"),
            type = "btn-outline-dark btn-xs",
            disabled = if (allow_rm) NULL else NA
          ),
          button(
            title = "Clear Filters", icon = shiny::icon("sync-alt", class = "fa-flip-horizontal"),
            onclick = .trigger_action_js("clear_step", list(step_id = step_id, run_flow = TRUE, reset = TRUE), ns = ns),
            type = "btn-outline-dark btn-xs"
          ),
          button(
            title = "Show / Edit", class = paste("cb_show_step", shinyGizmo::activatorClass),
            icon = shiny::icon("eye"),
            onclick = shinyGizmo::accordionEnrollOnClick(),
            disabled = if (active) NA else NULL,
            type = "btn-outline-dark btn-xs"
          ),
          if (run_button) {
            button(
              title = "Run", class = "cb_run_step",
              icon = shiny::icon("play"),
              onclick = .trigger_action_js(
                "run_step",
                list(step_id = step_id, run_flow = TRUE, reset = FALSE, update = c("input", "plot")),
                ns = ns
              ),
              disabled = NA,
              type = "btn-outline-dark btn-xs"
            )
          }
        )
      ),
      content_class = "panel-body card-body",
      content = .render_filters(cohort$get_source(), cohort, step_id, ns = ns),
      enroll_callback = FALSE,
      active = active
    )
  )

  call_filters(
    step_id, cohort, input, output, session
  )

  .update_data_stats(cohort$get_source(), step_id, cohort, session)
}

cat_nl <- function(...) {
  cat(...)
  cat(sep = "\n")
}

warning_nl <- function(...) {
  cat_nl("\033[38;5;203m", ..., "\033[39m")
}

error_nl <- function(...) {
  cat_nl("\033[31m", ..., "\033[39m")
}

print_state <- function(action, params) {
  if (!getOption("cb_verbose", default = FALSE)) {
    return(invisible(TRUE))
  }
  cat_nl(paste("===>", action))
  params %>%
    purrr::iwalk(~ cat_nl(paste0("  => ", .y, ": "), paste(.x, collapse = ", ")))
}

render_current_step <- function(cohort) {
  shiny::div(
    class = "cb_active_step",
    shiny::tags$span(class = "title", "active step: "),
    shiny::tags$span(class = "value", cohort$last_step_id())
  )
}

render_steps <- function(cohort, session, init = TRUE) {

  enable_panel(cohort, session)
  step_names <- names(cohort$get_step())
  active <- FALSE
  allow_rm <- FALSE
  for (step_name in step_names) {
    if (step_name == rev(step_names)[1]) {
      active <- TRUE
      if (length(step_names) > 1) {
        allow_rm <- TRUE
      }
    }
    cohort$run_step(step_name)
    # todo attach gui here?
    render_step(
      cohort,
      step_name,
      active,
      allow_rm,
      session$input, session$output, session
    )
  }

  if (init) {
    shiny::observeEvent(session$input$action, {
      action <- session$input$action

      gui_method <- switch(
        action$id,
        update_filter = gui_update_filter,
        add_step = gui_add_step,
        add_step_modal = gui_show_step_filter_modal,
        add_step_configure = gui_add_step_configured,
        rm_step = gui_rm_step,
        clear_step = gui_clear_step,
        update_step = gui_update_step,
        update_data_stats = gui_update_data_stats,
        show_repro_code = gui_show_repro_code,
        run_step = gui_run_step,
        show_state = gui_show_state,
        input_state = gui_input_state,
        restore_state = gui_restore_state,
        show_attrition = gui_show_attrition,
        show_help = gui_show_help
      )
      tryCatchLog::tryCatchLog(
        gui_method(cohort, action$params, session),
        error = function(e) {
          intro_message <- glue::glue("An error occured during execution of {sQuote(action$id)} method.")
          cat(intro_message, sep = "\n")
          message(cohort$get_state())
          if (action$id == "restore_state") {
            post_message <- "The previous state will be restored."
            session$sendCustomMessage(
              "show_alert",
              list(info = paste(intro_message, post_message, sep = "\n"))
            )
            action$params$state <- cohort$attributes$pre_restore_state
            gui_method(cohort, action$params, session)
          } else {
            session$sendCustomMessage(
              "show_alert",
              list(info = intro_message)
            )
          }
        }
      )

    }, ignoreInit = TRUE)
  }
}

empty_if_false <- function(condition, value, span = TRUE, empty = NULL) {
  if (!condition) {
    value <- empty
    span <- FALSE
  }
  if (span) {
    value <- shiny::tags$span(value, .noWS = no_ws)
  }
  return(value)
}

#' Generate structure of pre/post statistics
#'
#' The method exported only for custom extensions use.
#'
#' @param current Current step statistic value.
#' @param previous Previous step statistic value.
#' @param name Name displayed nearby the statistics output.
#' @param brackets If TRUE, statistics will be displayed in brackets.
#' @param percent Should current/previous ration in percentages be displayed?
#' @param stats Vector of "pre" and "post" defining which statistics should be returned.
#'   "pre" for previous, "post" for current and NULL for none.
#'
#' @export
.pre_post_stats <- function(current, previous, name, brackets = FALSE, percent = FALSE, stats = c("pre", "post")) {
  shiny::tags$span(
    empty_if_false(!missing(name), paste0(name, " ")),
    empty_if_false(brackets && length(stats), "("),
    empty_if_false("post" %in% stats, shiny::tags$span(class = "cb_delayed", current, .noWS = no_ws), span = FALSE),
    empty_if_false(length(stats) == 2, " / "),
    empty_if_false("pre" %in% stats, previous),
    empty_if_false(brackets && length(stats), ")"),
    empty_if_false(percent && length(stats) == 2, " ("),
    empty_if_false(
      percent && length(stats) == 2,
      shiny::tags$span(class = "cb_delayed", glue::glue("{round(100 * current / previous, 0)}%"), .noWS = no_ws),
      span = FALSE
    ),
    empty_if_false(percent && length(stats) == 2, ")"),
    .noWS = no_ws
  )
}

restore_attribute <- function(cohort, attribute, value) {
  if (is.null(cohort$attributes[[attribute]])) {
    cohort$attributes[[attribute]] <- value
  }
}

#' Return GUI layer methods for filter of specified type
#'
#' @description
#' For each filter type `.gui_filter` method should return a list of the below objects:
#'
#' \itemize{
#'   \item{input}{ UI structure defining filter input controllers.}
#'   \item{feedback}{ List defining feedback plot output.}
#'   \item{server}{ Optional serverside expression attached to filter panel (e.g. filter specific observers).}
#'   \item{update}{ An expression used for updating filter panel based on its configuration.}
#'   \item{post_stats}{ TRUE if post statistics are displayed in filter controller (e.g. for discrete filter).
#'     If FALSE, some operations are skipped which results with better performance.}
#'   \item{multi_input}{ TRUE if multiple input controllers are used for providing
#'     filter value (e.g. range input where both numericInput and sliderInput are used).
#'     If FALSE, some operations are skipped which results with better performance.
#'   }
#' }
#'
#' If you want to learn more about creating filter layers see `vignette("gui-filter-layer")`.
#'
#' @param filter Filter object.
#' @param ... Extra arguments passed to a specific method.
#' @return List consisting filter metadata and methods that allow to perform filter based operations.
#'     See `vignette("custom-filters")`.
#'
#' @name gui-filter-layer
#'
#' @seealso \link{source-gui-layer}
#' @export
.gui_filter <- function(filter, ...) {
  UseMethod(".gui_filter", filter)
}

#' Render filtering panels for all the filters included in Cohort
#'
#' The method exported only for custom extensions use.
#'
#' @details
#' Within the method you should define source data stats output (see \link{.update_data_stats}),
#' and define a loop that renders filtering panel for each filter (using \link{.render_filter}).
#'
#' @param source Source object.
#' @param cohort Cohort object.
#' @param step_id Id of the step.
#' @param ns Namespace function.
#'
#' @name rendering-filters
#' @param ... Extra arguments passed to a specific method.
#' @seealso \link{source-gui-layer}
#' @export
.render_filters <- function(source, ...) {
  UseMethod(".render_filters", source)
}

#' @rdname rendering-filters
#' @export
.render_filters.default <- function(source, cohort, step_id, ns, ...) {
  step <- cohort$get_step(step_id)
  shiny::tagList(
    shiny::htmlOutput(ns(paste0(step_id, "-stats")), class = "scb_data_stats"),
    step$filters %>%
      purrr::map(~ .render_filter(.x, step_id, cohort, ns = ns)) %>%
      shiny::div(class = "cb_filters", `data-step_id` = step_id)
  )
}

#' Include filtering panel in Shiny
#'
#' @description
#' The function returns filtering panel placeholder, you may use in you custom Shiny application.
#' Use in the UI part of your application.
#'
#'
#' @inheritParams demo_app
#' @param id Id of the module used to render the panel.
#' @param ... Extra attributes passed to the panel div container.
#' @export
cb_ui <- function(id, ..., state = FALSE, steps = TRUE, code = TRUE, attrition = TRUE, new_step = c("clone", "configure")) {
  ns <- shiny::NS(id)
  no_steps_class <- if (steps) "" else "cb_no_steps"
  no_state_class <- if (state) "" else "cb_no_state"
  no_code_class <- if (code) "" else "cb_no_code"
  no_attrition_class <- if (attrition) "" else "cb_no_attrition"

  new_step <- rlang::arg_match(new_step)
  add_step_action <- switch(new_step,
    "clone" = "add_step",
    "configure" = "add_step_modal"
  )

  shiny::addResourcePath(
    "shinyCohortBuilder",
    system.file("www", package = "shinyCohortBuilder")
  )

  js_scb <- file.path("shinyCohortBuilder", "scb.min.js")
  css_scb <- file.path("shinyCohortBuilder", "scb.min.css")
  if (!getOption("scb_minified", default = TRUE)) {
    js_scb <- file.path("shinyCohortBuilder", "scb.js")
    css_scb <- file.path("shinyCohortBuilder", "scb.css")
  }
  # https://github.com/sa-si-dev/tooltip
  tooltip_js <- file.path("shinyCohortBuilder", "tooltip.min.js")
  tooltip_css <- file.path("shinyCohortBuilder", "tooltip.min.css")

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", href = tooltip_css),
        shiny::tags$script(type = "text/javascript", src = tooltip_js),
        shiny::tags$link(rel = "stylesheet", href = css_scb),
        shiny::tags$script(type = "text/javascript", src = js_scb)
      )
    ),
    shiny::div(
      id = ns("cb_container"),
      class = paste("cb_container ", no_steps_class),
      `data-ns_prefix` = ns(""),
      ...,
      shiny::div(
        id = ns("cb_panel"),
        class = "cb_panel disabled",
        shiny::div(
          class = no_state_class,
          button(
            "Set state", icon = shiny::icon("sliders-h"),
            onclick = .trigger_action_js("input_state", ns = ns),
            style = "width: 49%"
          ),
          button(
            "Get state", icon = shiny::icon("stream"),
            onclick = .trigger_action_js("show_state", ns = ns),
            style = "width: 49%"
          )
        ),
        button(
          "Show Reproducible Code", icon = shiny::icon("code"),
          onclick = .trigger_action_js("show_repro_code", ns = ns),
          class = no_code_class
        ),
        button(
          "Show attrition data", icon = shiny::icon("project-diagram"),
          onclick = .trigger_action_js("show_attrition", ns = ns),
          class = no_attrition_class
        ),
        button(
          "Add Step", class = "cb_add_step", icon = shiny::icon("plus"),
          onclick = .trigger_action_js(add_step_action, ns = ns)
        )
      ),
      shinyGizmo::accordion(
        id = ns("cb_steps"),
        class = "cb_steps"
      )
    )
  )
}

bookmark_restore <- function(cohort, enable_bookmarking) {
  session <- cohort$attributes$session

  session$.__enclos_env__$private$registerBookmarkExclude(function() {
    session$ns(names(session$input))
  })

  if (isTRUE(enable_bookmarking != "disable")) {
    shiny::onBookmark(function(state) {
      state$values$cohort_state <- as.character(cohort$get_state(json = TRUE))
      state$values$cb_version <- as.character(utils::packageVersion("cohortBuilder"))
    })
    state <- `%:::%`("shiny", "getCurrentRestoreContext")()$asList()
    cohort_state <- state$values[[session$ns("cohort_state")]]
    # todo add error handling
    if (!is.null(cohort_state) && jsonlite::validate(cohort_state)) {
      cohort$restore(cohort_state, hook = list(
        pre = function(public, private, ...) {
          n_steps <- as.integer(public$last_step_id())
          if (n_steps == 0) {
            return(invisible(TRUE))
          }
          for (step_id in as.character(n_steps:1)) {
            cohort$remove_step(step_id)
          }
        },
        post = function(public, private, ...) {}
      ))
    }
  }
}

# todo init not directly defined here and auto setup

#' @rdname cb_ui
#' @inheritParams demo_app
#' @param cohort Cohort object storing filtering steps configuration.
#' @export
cb_server <- function(id, cohort, run_button = FALSE, stats = c("pre", "post"), feedback = FALSE,
                      enable_bookmarking = shiny::getShinyOption("bookmarkStore", default = "disable"),
                      show_help = TRUE) {

  if (!missing(id)) {
    cohort$attributes$id <- id
  }
  shiny::moduleServer(
    cohort$attributes$id,
    function(input, output, session) {

      restore_attribute(cohort, "session", session)
      restore_attribute(cohort, "run_button", run_button)
      restore_attribute(cohort, "stats", stats)
      restore_attribute(cohort, "feedback", feedback)
      restore_attribute(cohort, "show_help", show_help)

      shiny::onStop(function() {
        cohort$attributes$session <- NULL
        cohort$attributes$run_button <- NULL
        cohort$attributes$stats <- NULL
        cohort$attributes$feedback <- NULL
        cohort$attributes$show_help <- NULL
      }, session = session)

      bookmark_restore(cohort, enable_bookmarking)

      render_steps(cohort, cohort$attributes$session, init = TRUE)
    }
  )
}
