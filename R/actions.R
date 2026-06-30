#' Trigger filtering panel action
#'
#' @description
#'
#' The two functions that allow to trigger a specific filtering panel action directly
#' from Shiny server (.trigger_action) or application browser (.trigger_action_js)
#' attached to a specific JS event, e.g. onclick.
#'
#' Check Details section to see possible options.
#'
#' @details
#'
#' The list of possible actions:
#'
#' \itemize{
#' \item{\code{update_filter} - Calls `shinyCohortBuilder:::action_update_filter` that triggers filter arguments update.}
#' \item{\code{add_step} - Calls `shinyCohortBuilder:::action_add_step` that triggers adding a new filtering step (based on configuration of the previous one).}
#' \item{\code{rm_step} - Calls `shinyCohortBuilder:::action_rm_step` used to remove a selected filtering step.},
#' \item{\code{clear_step} - Calls `shinyCohortBuilder:::action_clear_step` used to clear filters configuration in selected step.}
#' \item{\code{update_data_stats} - Calls `shinyCohortBuilder:::action_update_data_stats` that is called to update data statistics. }
#' \item{\code{show_repro_code} - Calls `shinyCohortBuilder:::action_show_repro_code` that is used to show reproducible code. }
#' \item{\code{run_step} - Calls `shinyCohortBuilder:::action_run_step` used to trigger specific step data calculation. }
#' \item{\code{show_state} - Calls `shinyCohortBuilder:::action_show_state` that is used to show filtering panel state json. }
#' \item{\code{input_state} - Calls `shinyCohortBuilder:::action_input_state` that is used to generate modal in which filtering panel state can be provided (as json). }
#' \item{\code{restore_state} - Calls `shinyCohortBuilder:::action_restore_state` used for restoring filtering panel state based on provided json. }
#' \item{\code{show_attrition} - Calls `shinyCohortBuilder:::action_show_attrition` a method used to show attrition data plot(s).}
#' }
#'
#' Both `.trigger_action` and `.trigger_action_js` methods are exported for advanced use only.
#'
#' @param session Shiny session object.
#' @param action Id of the action.
#' @param params List of parameters passed to specific action method.
#' @param ns Namespace function (if used within Shiny modal).
#' @return No return value (`.trigger_action` - sends message to the browser) or
#'   character string storing JS code for sending input value to Shiny server (`.trigger_action_js`).
#'
#' @name trigger-action
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyCohortBuilder)
#'
#'   shiny::addResourcePath(
#'     "shinyCohortBuilder",
#'     system.file("www", package = "shinyCohortBuilder")
#'   )
#'   ui <- fluidPage(
#'     tags$head(
#'       shiny::tags$script(type = "text/javascript", src = file.path("shinyCohortBuilder", "scb.js"))
#'     ),
#'     tags$button(
#'       "Trigger action from UI", class = "btn btn-default",
#'       onclick = .trigger_action_js("uiaction", params = list(a = 1))
#'     ),
#'     actionButton("send", "Trigger action from server")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$send, {
#'       .trigger_action(session, "serveraction", params = list(a = 2))
#'     })
#'     observeEvent(input$action, {
#'       print(input$action)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
.trigger_action <- function(session, action, params = NULL) {
  session$sendCustomMessage("up_state", {
    list(id = action, params = params, ns_prefix = session$ns(""))
  })
}

#' @rdname trigger-action
#' @export
.trigger_action_js <- function(action, params = list(), ns = function(id) id) {
  state_val <- jsonlite::toJSON(
    list(id = action, params = params),
    auto_unbox = TRUE
  )
  glue::glue("Shiny.setInputValue('{ns('action')}', {state_val}, {{priority: 'event'}})")
}

#' Build a combined `"<step>-<filter>"` id
#' @param step_id,filter_id Step and filter ids.
#' @return The hyphen-joined id string.
#' @noRd
sf_id <- function(step_id, filter_id) {
  paste(step_id, filter_id, sep = "-")
}

#' Show a debug notification describing an action (when `scb_verbose`)
#'
#' Surfaces the action name and its params as a colour-coded Shiny notification;
#' a no-op unless the `scb_verbose` option is on.
#'
#' @param action Action name/label.
#' @param params Named list of parameters to display.
#' @param gui Whether the action originated from the GUI (changes the colour).
#' @param session Shiny session (defaults to the current reactive domain).
#' @return Invisibly `TRUE`; called for its notification side effect.
#' @noRd
input_state <- function(action, params, gui = TRUE, session = shiny::getDefaultReactiveDomain()) {
  if (!getOption("scb_verbose", default = FALSE)) {
    return(invisible(TRUE))
  }
  color = "#93a4d9"
  if (!gui) {
    color = "#c8dbbd"
  }
  if (!is.null(session)) {
    shiny::showNotification(
      shiny::tags$div(
        style = glue::glue("background-color: {color}"),
        shiny::tags$strong(action),
        shiny::br(),
      params |> purrr::imap(
        ~ shiny::tagList(
          shiny::tags$span(.y, ": ", paste(.x, collapse = ", ")),
          shiny::br()
        )
      )
    ), duration = 20, session = session)
  }
}

#' Save observer to user session
#'
#' The method used to store created observers (used to implement extra filter logic).
#' The saved observer are then destroyed when filtering step is removed which prevents
#' duplicated execution of accumulated observers.
#'
#' @param observer An `observe` or `observeEvent` to be saved.
#' @param id Id of the observer. Preferably prefixed with step_id.
#'    The saved observer is saved as `session$userData$observers[['<id>-observer']]` object.
#' @param session Shiny session object.
#' @return No return value, used for side effect which is saving the observer to
#'     `session$userData` object.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyCohortBuilder)
#'
#'   ui <- fluidPage(
#'     numericInput("power", "Power", min = 0, max = 10, value = 1, step = 1),
#'     numericInput("value", "Value", min = 0, max = 100, value = 2, step = 0.1),
#'     actionButton("add", "Observe the selected power"),
#'     actionButton("rm", "Stop observing the selected power")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$add, {
#'       .save_observer(
#'         observeEvent(input$value, {
#'            print(input$value ^ input$power)
#'         }),
#'         as.character(input$power),
#'         session = session
#'       )
#'     }, ignoreInit = TRUE)
#'
#'     observeEvent(input$rm, {
#'       id <- paste0(input$power, "-observer")
#'       session$userData$observers[[id]]$destroy()
#'       session$userData$observers[[id]] <- NULL
#'     }, ignoreInit = TRUE)
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
.save_observer <- function(observer, id, session) {
  # todo save in key value storage for user session?
  session$userData$observers[[session$ns(paste0(id, "-observer"))]] <- observer
}

#' Remove a step's inputs, outputs and observers from the session
#'
#' Tears down all session state associated with a step id (input values, output
#' bindings, saved observers and the rendered-filters registry) so a removed or
#' rebuilt step does not leave stale reactives behind.
#'
#' @param id Step id whose session state should be cleared.
#' @param .session Shiny session object.
#' @return Invisibly `NULL`; called for its session-cleanup side effects.
#' @noRd
clear_step_data <- function(id, .session) {
  ns <- .session$ns

  invisible(
    lapply(grep(paste0("^", id, "-"), names(.session$input), value = TRUE), function(i) {
      .subset2(.session$input, "impl")$.values$remove(i)
    })
  )
  output_names <- grep(
    paste0("^", ns(id), "-"),
    names(.subset2(.session$output, "impl")$.__enclos_env__$private$.outputs),
    value = TRUE
  )
  lapply(output_names, function(i) {
    .session$output[[i]] <- NULL
  })

  observer_names <- grep(
    paste0("^", ns(id), "-"),
    names(.session$userData$observers),
    value = TRUE
  )
  lapply(observer_names, function(i) {
    .session$userData$observers[[i]]$destroy()
  })

  rendered_filters_idx <- grep(
    paste0("^", ns(id), "-"),
    .session$userData$rendered_filters
  )
  .session$userData$rendered_filters <- .session$userData$rendered_filters[-rendered_filters_idx]
}

#' Send output rendering
#'
#' Functional approach to rendering output.
#' Equivalent of `output[[name]] <- rendering`.
#'
#' @param name Name of the output to be rendered
#' @param rendering Rendering expression to be sent.
#' @param session Shiny session object.
#' @return No return value, used for side effect which is assigning rendering to the output object.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyCohortBuilder)
#'
#'   rendering <- function(x_max) {
#'     renderPlot({
#'       x <- seq(0, x_max, by = 0.01)
#'       plot(x, sin(x), type = "l")
#'     })
#'   }
#'
#'   ui <- fluidPage(
#'     numericInput("xmax", "X Axis Limit", min = 0, max = 10, value = pi),
#'     plotOutput("out")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$xmax, {
#'       .sendOutput("out", rendering(input$xmax))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
.sendOutput <- function(name, rendering, session = shiny::getDefaultReactiveDomain()) {
  value <- if (is.null(formals(rendering))) {
    rendering()
  } else {
    rendering(shinysession = session, name = name)
  }
  session$output[[name]] <- function() value
}

#' Toggle a CSS class on a filter's container (client-side)
#' @param step_id,filter_id Ids identifying the filter.
#' @param show Whether to add (`TRUE`) or remove (`FALSE`) the class.
#' @param class CSS class to toggle.
#' @param session Shiny session.
#' @param child Optional child selector within the filter the class applies to.
#' @return Invisibly `NULL`; sends the `update_filter_class` custom message.
#' @noRd
ui_update_filter_class <- function(step_id, filter_id, show, class, session, child = ".cb_filter_content") {
  session$sendCustomMessage(
    "update_filter_class",
    list(
      step_id = step_id, filter_id = filter_id,
      show = show, ns_prefix = session$ns(""),
      class = class, child = child
    )
  )
}

#' Refresh a single filter's input and/or feedback plot
#'
#' Dispatches the requested UI updates for one filter: re-runs its input
#' `update()` and, in stats mode, toggles the "no data" gate and re-renders the
#' feedback plot. Which pieces run is driven by the `update` vector (`"input"`,
#' `"plot"`, `"post_input"`, `"multi_input"`, `"force_input"`).
#'
#' @param cohort The cohort object.
#' @param step_id,filter_id Ids identifying the filter.
#' @param update Character vector of UI pieces to refresh.
#' @param reset Whether the filter value should be reset to its default.
#' @param session Shiny session.
#' @return Invisibly `NULL`; called for its UI side effects.
#' @noRd
ui_update_filter <- function(cohort, step_id, filter_id, update, reset, session) {
  filter <- cohort$get_filter(step_id, filter_id)
  updated_input <- FALSE
  updated_plot <- FALSE

  if (("post_input" %in% update) && !identical(filter@private$gui$post_stats, FALSE)) {
    update <- c(update, "input")
  }
  if (("multi_input" %in% update) && filter@private$gui$multi_input) {
    update <- c(update, "input")
  }
  if ("force_input" %in% update) {
    update <- c(update, "input")
  }

  if ("input" %in% update) {
    filter@private$gui$update(
      filter,
      session,
      sf_id(step_id, filter_id),
      cohort,
      reset = reset
    )

    updated_input <- TRUE
  }
  if ("plot" %in% update) {
    render <- resolve_render_mode(filter, cohort)
    # The "no data" gate and feedback plots only apply in stats mode. In domain
    # mode there is no stats to read and no feedback plot to refresh.
    if (render$mode == "stats") {
      show <- TRUE
      if (!cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data")) {
        show <- FALSE
      }
      ui_update_filter_class(
        step_id, filter_id, show, "cb_no_data", session,
        child = ".cb_filter_content .cb_no_data_placeholder"
      )
      if (isTRUE(render$feedback)) {
        updated_plot <- TRUE
        ui_update_plot(step_id, filter_id, cohort, session) # todo optmize to not extract filter again inside plot
      }
    }
  }
  input_state(
    "ui_update_filter",
    list(
      step_id = step_id, filter_id = filter_id, reset = reset,
      updated_plot = updated_plot, updated_input = updated_input
    )
  )
  print_state(
    "ui_update_filter",
    list(
      step_id = step_id, filter_id = filter_id, reset = reset,
      updated_plot = updated_plot, updated_input = updated_input
    )
  )
}

#' Apply [ui_update_filter()] to every active filter in a step
#' @param cohort The cohort object.
#' @param step_id Id of the step.
#' @param reset Whether filter values should be reset.
#' @param update Character vector of UI pieces to refresh.
#' @param exclude Filter ids to skip.
#' @param session Shiny session.
#' @return Invisibly `NULL`; called for its UI side effects.
#' @noRd
ui_update_filters_loop <- function(cohort, step_id, reset, update, exclude = character(0), session) {
  filter_ids <- cohort$list_active_filters(step_id)

  updated_inputs <- updated_plots <- character(0)
  input_state(
    "update_filters_loop",
    list(step_id = step_id, reset = reset, update = update, exclude = exclude)
  )
  print_state(
    "update_filters_loop",
    list(step_id = step_id, reset = reset, update = update, exclude = exclude)
  )
  for (filter_id in filter_ids) {
    if (filter_id %in% exclude) {
      next()
    }
    ui_update_filter(cohort, step_id, filter_id, update, reset, session)
  }
}

#' Re-render a filter's feedback plot output
#' @param step_id,filter_id Ids identifying the filter.
#' @param cohort The cohort object.
#' @param session Shiny session.
#' @return Invisibly `NULL`; reassigns the feedback output.
#' @noRd
ui_update_plot <- function(step_id, filter_id, cohort, session) {
  ns <- session$ns

  print_state("update_plot", list(step_id = step_id, filter_id = filter_id))
  input_state("update_plot", list(step_id = step_id, filter_id = filter_id))

  filter <- cohort$get_filter(step_id, filter_id)
  no_data <- cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data") == 0
  feedback <- filter@private$gui$feedback(filter, sf_id(step_id, filter_id), cohort, no_data)
  session$output[[feedback$plot_id]] <- feedback$render_fun
}


overwrite_input_handler <- list(
  "sw.airdatepicker" = function() ...
)

#' Coerce a raw input value via its Shiny input handler
#'
#' Applies the registered (or overridden) Shiny input handler for a binding so a
#' value arriving from the browser is converted to the R type the filter expects
#' (with special handling for air datepicker datetime ranges).
#'
#' @param val Raw input value from the browser.
#' @param binding Input binding name (selects the handler).
#' @return The coerced value.
#' @noRd
input_val_handler <- function(val, binding) {
  handler <- NULL
  if (!length(binding)) {
    binding <- ""
  }
  if (binding %in% names(overwrite_input_handler)) {
    handler <- overwrite_input_handler
  } else if (binding != "" && !is.na(binding)) {
    handler <- `%:::%`("shiny", "inputHandlers")$get(binding)
  }
  if (!is.null(handler)) {
    val <- handler(val)
    if ("air.datetime" %in% binding) {
      if (is.null(val)) return(c(Inf, -Inf))
      if (length(val) == 1) return(c(val, val))
    }
    return(val)
  }
  if (is.list(val)) {
    if (is.null(names(val))) {
      return(unlist(val, recursive = TRUE))
    }
    return(val)
  }

  val
}

#' Normalise a changed-input payload into `update_filter` arguments
#'
#' Coerces the raw input value (via [input_val_handler()]) and reshapes the
#' browser payload into the named list of arguments expected by
#' `cohort$update_filter()`.
#'
#' @param changed_input The raw changed-input list from the browser.
#' @param step_id,filter_id Ids identifying the filter.
#' @param cohort The cohort object.
#' @param update_active Whether the change toggles the filter's active state.
#' @return A named list of arguments for `cohort$update_filter()`.
#' @noRd
convert_input_value <- function(changed_input, step_id, filter_id, cohort, update_active) {
  # todo handle case when no value parameter defined (some filters can work like that)

  changed_input[changed_input$input_name] <- list(
    input_val_handler(changed_input$input_value, changed_input$binding)
  )
  update_keep_na <- changed_input$input_name == "keep_na"
  changed_input$input_name <- NULL
  changed_input$input_value <- NULL
  changed_input$update <- NULL

  return(changed_input)
}

# GUI action handlers
#
# Each `action_*` function below handles one GUI action routed from the single
# `input$action` observer set up in `render_steps()` (see `.trigger_action()` /
# `.trigger_action_js()`). They share the signature `(cohort, changed_input,
# session)`, where `changed_input` is the action's `params` list, and act on the
# cohort / session for their side effects (returning invisibly).

#' Action: apply a changed filter input to the cohort
#'
#' Converts the browser payload and calls `cohort$update_filter()`; downstream UI
#' refresh and cascading are handled by [post_update_filter_hook()].
#'
#' @param cohort The cohort object.
#' @param changed_input The action params (changed-input payload).
#' @param session Shiny session.
#' @return Invisibly; called for its side effects.
#' @noRd
action_update_filter <- function(cohort, changed_input, session) {

  run_on_request <- !is_none(cohort$attributes$run_button)

  update_active <- changed_input$input_name == "active"
  update <- changed_input$update

  step_id <- changed_input$step_id
  filter_id <- changed_input$filter_id

  print_state("update_filter", changed_input)
  input_state("update_filter", changed_input)

  changed_input <- convert_input_value(changed_input, step_id, filter_id, cohort, update_active)
  changed_input$hook_args <- list(
    pre = list(),
    post = list(update = update)
  )
  do.call(
    cohort$update_filter,
    changed_input
  )
}

#' Insert a filter's inner content if not already rendered
#'
#' Lazily renders the feedback/inputs content for a filter into its container,
#' guarding against double-rendering via the session's rendered-filters registry.
#'
#' @param step_id,filter_id Ids identifying the filter.
#' @param cohort The cohort object.
#' @param session Shiny session.
#' @return Invisibly `TRUE` if content was inserted, `FALSE` if already rendered.
#' @noRd
ui_insert_filter_content <- function(step_id, filter_id, cohort, session) {
  ns <- session$ns
  step_filter_id <- sf_id(step_id, filter_id)
  rendered_already <- ns(step_filter_id) %in% session$userData$rendered_filters
  if (rendered_already) {
    return(invisible(FALSE))
  }

  filter <- cohort$get_filter(step_id, filter_id)

  filter_content <- render_filter_content(step_filter_id, filter, cohort, ns)
  shiny::insertUI(
    selector = paste0("#", ns(step_filter_id), " .cb_filter_content"),
    where = "beforeEnd",
    ui = filter_content,
    immediate = TRUE,
    session = session
  )
  return(invisible(TRUE))
}

#' Render and insert a whole filter into its step
#'
#' Attaches the GUI to the filter, renders it via [.render_filter()], inserts it
#' at the source-defined position ([.filter_position()]), and re-validates the
#' step's filter groups client-side.
#'
#' @param step_id,filter_id Ids identifying the filter.
#' @param cohort The cohort object.
#' @param session Shiny session.
#' @return Invisibly `NULL`; called for its UI side effects.
#' @noRd
ui_insert_filter <- function(step_id, filter_id, cohort, session) {

  ns <- session$ns
  step_filter_id <- sf_id(step_id, filter_id)

  cohort$modify(function(public, private) {
    private$steps[[step_id]]$filters[[filter_id]] <- attach_filter_gui(
      private$steps[[step_id]]$filters[[filter_id]]
    )
  })
  filter <- cohort$get_filter(step_id, filter_id)
  step_filter_target_id <- .filter_position(cohort$get_source(), step_id = step_id, filter = filter, ns = ns)

  filter_ui <- .render_filter(filter, step_id, cohort, ns)

  shiny::insertUI(
    selector = step_filter_target_id,
    where = "beforeEnd",
    ui = filter_ui,
    immediate = TRUE,
    session = session
  )
  session$sendCustomMessage(
    "validate_filter_groups",
    list(step_id = step_id, ns_prefix = session$ns(""))
  )
}

#' Remove a filter's UI from its step
#' @param step_id,filter_id Ids identifying the filter.
#' @param cohort The cohort object.
#' @param session Shiny session.
#' @return Invisibly `NULL`; removes the filter UI and re-validates groups.
#' @noRd
ui_remove_filter <- function(step_id, filter_id, cohort, session) {
  ns <- session$ns
  step_filter_id <- sf_id(step_id, filter_id)
  session$userData$rendered_filters <- setdiff(
    session$userData$rendered_filters,
    ns(step_filter_id)
  )

  shiny::removeUI(
    selector = glue::glue("#{ns(step_filter_id)}"),
    immediate = TRUE,
    session = session
  )
  session$sendCustomMessage(
    "validate_filter_groups",
    list(step_id = step_id, ns_prefix = session$ns(""))
  )
}

#' Action: remove the last filtering step
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_rm_step <- function(cohort, changed_input, session) {
  # todo make sure to diasble delete button when source is updated
  print_state("rm_step", changed_input)
  input_state("rm_step", changed_input)
  cohort$remove_step(run_flow = TRUE)
}

#' Action: open the "manage last step filters" modal
#'
#' Shows a multi-select of the source's available filters (pre-selected with the
#' last step's current filters). Falls back to [action_add_step()] when no
#' available filters are configured.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_manage_step_modal <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("manage_step_modal", changed_input)
  input_state("manage_step_modal", changed_input)

  available_filters <- cohort$attributes$available_filters

  if (length(available_filters) == 0) {
    warning_nl("`available_filters` was not defined, configure step will not be working. Cloning last step.")
    return(action_add_step(cohort, changed_input, session))
  }

  choices <- .available_filters_choices(cohort$get_source(), cohort)
  selected <- cohort$get_step(cohort$last_step_id())$filters |>
    purrr::map_chr(~.x@id)
  if (length(selected) == 0) {
    selected <- NULL
  }

  shiny::showModal(
    shiny::modalDialog(
      shinyWidgets::virtualSelectInput(
        ns("manage_step"),
        label = "Choose filters",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        html = TRUE,
        search =  TRUE,
        selectAllOnlyVisible = TRUE,
        zIndex = 9999
      ),
      shiny::tags$script(
        shiny::HTML(
          glue::glue(
            "$('#{ns('manage_step')}').change(function() {{",
            "$('#{ns('manage_step_configured')}').attr('disabled', !(this.value.length > 0))}})"
          )
        )
      ),
      footer = shiny::tagList(
        # `data-bs-dismiss` is attached after construction because
        # shinyGizmo::valueButton() does not accept extra spliced tag
        # attributes via `...` (it errors with "invalid argument type").
        htmltools::tagAppendAttributes(
          shinyGizmo::valueButton(
            inputId = ns("manage_step_configured"),
            label = "Accept",
            selector = paste0("[data-id=\"", ns("manage_step"), "\"]"),
            onclick = .trigger_action_js("manage_step_configure", ns = ns),
            disabled = NA
          ),
          !!!bs_data_attr("dismiss", "modal")
        ),
        shiny::modalButton("Dismiss")
      ),
      title = "Manage last step filters",
      size = "m",
      easyClose = TRUE
    )
  )
}

#' Action: apply the chosen filters to the last step
#'
#' Diffs the selected filters against the step's current filters, then adds /
#' removes filters accordingly (pre-warming stats only when stats are enabled),
#' and runs the step in immediate mode.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_manage_step_configured <- function(cohort, changed_input, session) {

  run_on_request <- !is_none(cohort$attributes$run_button)

  print_state("manage_step", changed_input)
  input_state("manage_step", changed_input)

  step_id <- cohort$last_step_id()
  chosen_ids <- session$input[["manage_step"]]
  current_ids <- cohort$get_filter(step_id) |> purrr::map_chr(~.x@id)
  to_rm_ids <- setdiff(current_ids, chosen_ids)
  to_add_ids <- setdiff(chosen_ids, current_ids)

  available_filters <- cohort$attributes$available_filters
  available_filter_ids <- purrr::map_chr(available_filters, ~.x@id)
  available_filters <- stats::setNames(available_filters, available_filter_ids)

  if (length(available_filters) == 0) {
    stop("`available_filters` is not defined, configuring step will not be working.")
  }

  to_add_filters <- available_filters[to_add_ids]
  for (filter in to_add_filters) {
    filter_state <- get_filter_params(filter)
    cohort$add_filter(
      filter = do.call(cohortBuilder::filter, filter_state),
      step_id = step_id,
      run_flow = FALSE
    )
    # Pre-warm the filter stats only when statistics are in use. With stats
    # disabled the filter renders from its domain, so avoid a source scan.
    if (!is.null(cohort$attributes$stats)) {
      cohort$update_stats(step_id, filter@id, state = "pre")
    }
  }

  for (filter_id in to_rm_ids) {
    cohort$remove_filter(
      filter_id = filter_id,
      step_id = step_id,
      run_flow = FALSE
    )
  }

  if (!run_on_request) {
    cohort$run_step(step_id)
  }
}

#' Action: run the filtering flow from a given step
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_run_step <- function(cohort, changed_input, session) {
  print_state("run_step", changed_input)
  input_state("run_step", changed_input)

  cohort$run_flow(min_step = changed_input$step_id)
}

#' Action: show the cohort state JSON in a modal
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_show_state <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_state", changed_input)
  input_state("show_state", changed_input)

  shiny::showModal(shiny::modalDialog(
    size = "l",
    title = "Cohort state",
    shiny::tags$code(
      cohort$get_state(json = TRUE) |>
        shiny::HTML()
    ),
    easyClose = TRUE
  ))
}

#' Read a state value from an uploaded file or pasted string
#' @param filepath A Shiny `fileInput` value (or `NULL`); takes precedence.
#' @param string A pasted JSON string (or `NULL`/`""`).
#' @return The file contents as a single string, the pasted string, or `NULL`.
#' @noRd
file_string_value <- function(filepath, string) {
  if (!is.null(filepath)) {
    return(
      paste0(readLines(con = filepath$datapath), collapse = "")
    )
  }
  if (!is.null(string) && !identical(string, "")) {
    return(string)
  }
}

#' Action: open the "provide state" modal (file upload or pasted JSON)
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_input_state <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("input_state", changed_input)
  input_state("input_state", changed_input)
  suff_id <- cohort$attributes$id
  file_id <- paste0(suff_id, "_file_state")
  string_id <- paste0(suff_id, "_string_state")

  shiny::showModal(shiny::modalDialog(
    size = "l",
    title = "Cohort state",
    shiny::tagList(
      shiny::fileInput(ns(file_id), "Choose json file", accept = ".json"),
      divider("OR"),
      shiny::textAreaInput(ns(string_id), "Paste json state")
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Confirm") |>
        shiny::tagAppendAttributes(
          onclick = .trigger_action_js(
            "restore_state",
            ns = ns
          )
        ),
      shiny::modalButton("Dismiss")
    )
  ))
}

#' Action: restore the cohort from a provided state
#'
#' Restores from `changed_input$state` when present, otherwise from the modal's
#' file/string inputs.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_restore_state <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("restore_state", changed_input)
  input_state("restore_state", changed_input)

  suff_id <- cohort$attributes$id
  file_id <- paste0(suff_id, "_file_state")
  string_id <- paste0(suff_id, "_string_state")

  if (!is.null(changed_input$state)) {
    state <- changed_input$state
  } else {
    state <- file_string_value(session$input[[file_id]], session$input[[string_id]])
  }
  cohort$restore(state)
}

#' Action: add a step by cloning the previous one
#'
#' Copies the last step (or seeds the first step from available filters).
#' Triggers a flow run in immediate mode; in run-button mode the new step stays
#' pending until the user runs it. UI insertion is handled by
#' [post_add_step_hook()].
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_add_step <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("add_step", changed_input)
  input_state("add_step", changed_input)
  available_filters <- cohort$attributes$available_filters

  # In run_button mode the new step renders from the parent's copied snapshot
  # (seeded by add_step) and stays pending until the user runs the flow, so we
  # must not force a flow here.
  run_on_request <- !is_none(cohort$attributes$run_button)
  run_flow <- !run_on_request

  if (length(cohort$get_step()) == 0 && length(available_filters) > 0) {
    cohort$copy_step(
      filters = available_filters,
      run_flow = run_flow
    )
  } else {
    cohort$copy_step(run_flow = run_flow)
  }

  # gui actions are handled via post_add_step_hook hook

}

#' Action: add a step containing the user-selected filters
#'
#' Adds a step built from the filters chosen in the "configure new step" modal,
#' falling back to [action_add_step()] when no available filters are configured.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_add_step_configured <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("action_add_step_configured", changed_input)
  input_state("action_add_step_configured", changed_input)

  chosed_filters <- session$input[["configure_step"]]
  available_filters <- cohort$attributes$available_filters

  if (length(available_filters) == 0) {
    warning_nl("`available_filters` was not defined, configure step will not be working. Cloning last step.")
    return(action_add_step(cohort, changed_input, session))
  }

  filters <- available_filters |>
    purrr::keep(function(x) {x@id %in% chosed_filters})

  run_on_request <- !is_none(cohort$attributes$run_button)

  cohort$copy_step(
    filters = filters,
    run_flow = !run_on_request
  )

  # gui actions are handled via post_add_step_hook hook
}

#' Action: open the "configure new step" filter-picker modal
#'
#' Shows a multi-select of available filters for building a new step; falls back
#' to [action_add_step()] when none are configured.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_show_step_filter_modal <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("add_step_modal", changed_input)
  input_state("add_step_modal", changed_input)

  available_filters <- cohort$attributes$available_filters

  if (length(available_filters) == 0) {
    warning_nl("`available_filters` was not defined, configure step will not be working. Cloning last step.")
    return(action_add_step(cohort, changed_input, session))
  }

  choices <- .available_filters_choices(cohort$get_source(), cohort)

  shiny::showModal(
    shiny::modalDialog(
      shinyWidgets::virtualSelectInput(
        ns("configure_step"),
        label = "Choose filters",
        choices = choices,
        multiple = TRUE,
        html = TRUE,
        search =  TRUE,
        selectAllOnlyVisible = TRUE,
        zIndex = 9999
      ),
      shiny::tags$script(
        shiny::HTML(
          glue::glue(
            "$('#{ns('configure_step')}').change(function() {{",
            "$('#{ns('add_step_configured')}').attr('disabled', !(this.value.length > 0))}})"
          )
        )
      ),
      footer = shiny::tagList(
        # `data-bs-dismiss` is attached after construction because
        # shinyGizmo::valueButton() does not accept extra spliced tag
        # attributes via `...` (it errors with "invalid argument type").
        htmltools::tagAppendAttributes(
          shinyGizmo::valueButton(
            inputId = ns("add_step_configured"),
            label = "Accept",
            selector = paste0("[data-id=\"", ns("configure_step"), "\"]"),
            onclick = .trigger_action_js("add_step_configure", ns = ns),
            disabled = NA
          ),
          !!!bs_data_attr("dismiss", "modal")
        ),
        shiny::modalButton("Dismiss")
      ),
      title = "Configure new step",
      size = "m",
      easyClose = TRUE
    )
  )
}

#' Add/remove a step's "pending" styling client-side
#' @param step_id Id of the step.
#' @param action Either `"add"` or `"remove"`.
#' @param session Shiny session.
#' @return Invisibly `NULL`; sends the `update_class` custom message.
#' @noRd
ui_trigger_pending_state <- function(step_id, action, session) {
  session$sendCustomMessage(
    "update_class",
    list(
      step_id = step_id, class = "pending", action = action,
      disable = ".cb_run_step", ns_prefix = session$ns("")
    )
  )
}


#' Preserve leading whitespace by converting it to `&nbsp;` entities
#'
#' Used when rendering highlighted reproducible code so indentation survives HTML
#' whitespace collapsing.
#'
#' @param string A single line of (HTML) text.
#' @return The line with leading spaces replaced by non-breaking spaces.
#' @noRd
add_trailing_space <- function(string) {
  n_spaces <- nchar(regmatches(string, regexpr("^\\s+", string)))
  if (!length(n_spaces)) {
    return(string)
  }
  gsub("^\\s+", paste(rep("&nbsp", n_spaces), collapse = ""), string)
}

#' Action: show syntax-highlighted reproducible code in a modal
#'
#' Renders the cohort's generated code with a copy-to-clipboard button.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_show_repro_code <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_code", changed_input)
  input_state("show_code", changed_input)

  shiny::showModal(shiny::modalDialog(
    size = "xl",
    title = "Reproducible code",
    shiny::tags$pre(
      .noWS = c("after-begin", "before-end"),
      shiny::tags$code(
        id = "scb-reproducible-code",
        class = "hl background",
        cohort$get_code(width = I(120), output = FALSE)$text.tidy |>
          highr::hi_html() |>
          purrr::map_chr(add_trailing_space) |>
          paste(collapse = "\n") |>
          shiny::HTML()
      )
    ),
    easyClose = TRUE,
    footer = shiny::tagList(
      button(
        id = "scb-copy-to-clipboard",
        label = "",
        title = "Copy to Clipboard",
        icon = shiny::icon("copy"),
        onclick = '
        $(this).hide();
        navigator.clipboard.writeText($("#scb-reproducible-code")[0].innerText);
        $("#scb-copy-to-clipboard-tooltip").show().fadeOut(2000, function() {
            $("#scb-copy-to-clipboard").show();
        });
      '
      ),
      button(
        id = "scb-copy-to-clipboard-tooltip",
        icon = shiny::icon("check"),
        style = "border: 0px; background: none; display: none",
      ),
      shiny::modalButton("Dismiss")
    )
  ))
}

#' Generate output of attrition plot
#'
#' @description
#' The method should return list of two object:
#' \itemize{
#'   \item{\code{render} - Rendering expression of attrition output.}
#'   \item{\code{output} - Output expression related to rendering (with id equal to `id` parameter).}
#' }
#' For example:
#' \preformatted{
#'   list(
#'     render = shiny::renderPlot({
#'       cohort$show_attrition()
#'     }),
#'     output = shiny::plotOutput(id)
#'   )
#' }
#'
#' @param source Source object.
#' @param id Id of attrition output.
#' @param cohort Cohort object.
#' @param session Shiny session object.
#' @param ... Extra arguments passed to specific method.
#' @return List of two objects: `render` and `output` defining rendering and
#'     output placeholder for step attrition plot feature.
#'
#' @examples
#' if (interactive()) {
#'   library(magrittr)
#'   library(shiny)
#'   library(cohortBuilder)
#'   library(shinyCohortBuilder)
#'
#'   coh <- cohort(
#'     set_source(as.tblist(librarian)),
#'     step(
#'       filter(
#'         "range", id = "copies", dataset = "books",
#'         variable = "copies", range = c(5, 12)
#'       )
#'     ),
#'     step(
#'       filter(
#'         "range", id = "copies", dataset = "books",
#'         variable = "copies", range = c(6, 8)
#'       )
#'     )
#'   ) |> run()
#'
#'   ui <- fluidPage(
#'     div(id = "attrition")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rendering <- .step_attrition(
#'       coh$get_source(), id = "attr", cohort = coh, session = session, dataset = "books"
#'     )
#'     insertUI("#attrition", ui = rendering$output)
#'     output$attr <- rendering$render
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name rendering-step-attrition
#' @seealso \link{source-gui-layer}
#' @export
.step_attrition <- function(source, ...) {
  UseMethod(".step_attrition", source)
}

#' @rdname rendering-step-attrition
#' @export
.step_attrition.default <- function(source, id, cohort, session, ...) {
  ns <- session$ns

  list(
    render = shiny::renderPlot({
      cohort$show_attrition(...)
    }),
    output = shiny::plotOutput(id)
  )
}

#' Method for generating custom attrition output
#'
#' When method is defined for selected source, the output is displayed in attrition modal tab.
#'
#' @details
#' Similar to \link{.step_attrition} the method should return list of `render` and `output` expressions.
#'
#' @param source Source object.
#' @param ... Extra arguments passed to specific method.
#' @return List of two objects: `render` and `output` defining rendering and
#'    output placeholder for custom attrition plot feature.
#'
#' @examples
#' if (interactive()) {
#'   library(magrittr)
#'   library(shiny)
#'   library(cohortBuilder)
#'   library(shinyCohortBuilder)
#'
#'   .custom_attrition.tblist <- function(source, id, cohort, session, ...) {
#'     ns <- session$ns
#'     choices <- names(source$dtconn)
#'
#'     list(
#'       render = shiny::renderPlot({
#'         cohort$show_attrition(dataset = session$input$attrition_input)
#'       }),
#'       output = shiny::tagList(
#'         shiny::h3("Step-wise Attrition Plot"),
#'         shiny::selectInput(ns("attrition_input"), "Choose dataset", choices),
#'         shiny::plotOutput(id)
#'       )
#'     )
#'   }
#'   coh <- cohort(
#'     set_source(as.tblist(librarian)),
#'     step(
#'       filter(
#'         "range", id = "copies", dataset = "books",
#'         variable = "copies", range = c(5, 12)
#'       )
#'     ),
#'     step(
#'       filter(
#'         "range", id = "copies", dataset = "books",
#'         variable = "copies", range = c(6, 8)
#'       )
#'     )
#'   ) |> run()
#'
#'   ui <- fluidPage(
#'     div(id = "attrition")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rendering <- .custom_attrition(
#'       coh$get_source(), id = "attr", cohort = coh, session = session, dataset = "books"
#'     )
#'     insertUI("#attrition", ui = rendering$output)
#'     output$attr <- rendering$render
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name rendering-custom-attrition
#' @seealso \link{source-gui-layer}
#' @export
.custom_attrition <- function(source, ...) {
  UseMethod(".custom_attrition", source)
}

#' Thin wrapper over `shiny::tabsetPanel()`
#' @param ... Tab panels.
#' @param id,selected,type,header,footer Passed through to `shiny::tabsetPanel()`.
#' @return A tabset `shiny.tag`.
#' @noRd
navs <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"),
                 header = NULL,  footer = NULL) {
  shiny::tabsetPanel(
    ..., id = id, selected = selected, type = type, header = header, footer = footer
  )
}

#' Action: show the attrition plot(s) in a modal
#'
#' Renders the step attrition plot (and a custom attrition tab when a
#' `.custom_attrition` method exists for the source). Requires statistics; warns
#' and aborts when `stats` is disabled.
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_show_attrition <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_attrition", changed_input)
  input_state("show_attrition", changed_input)

  # Attrition is computed from per-step statistics. When stats are disabled there
  # is nothing to show, and computing it would force a source scan.
  if (is.null(cohort$attributes$stats)) {
    shiny::showNotification(
      "Attrition requires statistics; enable `stats` to use this feature.",
      type = "warning"
    )
    return(invisible(NULL))
  }

  custom_method <- cohortBuilder::.get_method(
    paste0(".custom_attrition.", class(cohort$get_source())[1])
  )

  step_attrition_content <- .step_attrition(
    source = cohort$get_source(),
    id = ns("step_attrition_plot"),
    cohort = cohort,
    session = session
  )

  session$output$step_attrition_plot <- step_attrition_content$render

  ui <- step_attrition_content$output
  if (!is.null(custom_method)) {
    custom_attrition_content <- custom_method(
      source = cohort$get_source(),
      id = ns("custom_attrition_plot"),
      cohort = cohort,
      session = session
    )
    session$output$custom_attrition_plot <- custom_attrition_content$render
    ui <- navs(
      shiny::tabPanel("Step", step_attrition_content$output),
      shiny::tabPanel("Custom", custom_attrition_content$output)
    )
  }

  shiny::showModal(shiny::modalDialog(
    size = "l",
    title = "Cohort attrition",
    ui,
    easyClose = TRUE
  ))
}

no_ws <- c("before", "after", "outside", "after-begin", "before-end", "inside")

#' Render source data related statistics
#'
#' @description
#'
#' The function should assign rendering that displays data source statistics to the valid output.
#' By default, the output is placed within \link{.render_filters} method.
#'
#' @details
#' When rendering the output, a good practice is to use the data statistics available with
#' `cohort$get_stats(step_id)`.
#' This way, you omit running additional computations which results with performance improvement.
#'
#' @param source Source object.
#' @param step_id Id if filtering step.
#' @param cohort Cohort object.
#' @param session Shiny session object.
#' @param ... Extra arguments passed to a specific method.
#' @return No return value, used for side effect which assigning Cohort data
#'     statistics to the `output` object.
#'
#' @examples
#' if (interactive()) {
#'   library(magrittr)
#'   library(shiny)
#'   library(cohortBuilder)
#'   library(shinyCohortBuilder)
#'
#'   ui <- fluidPage(
#'     sliderInput("step_two_max", "Max step two copies", min = 6, max = 12, value = 8),
#'     uiOutput("2-stats_books")
#'   )
#'
#'   server <- function(input, output, session) {
#'     coh <- cohort(
#'       set_source(as.tblist(librarian)),
#'       step(
#'         filter(
#'           "range", id = "copies", dataset = "books",
#'           variable = "copies", range = c(5, 12)
#'         )
#'       ),
#'       step(
#'         filter(
#'           "range", id = "copies", dataset = "books",
#'           variable = "copies", range = c(6, 8)
#'         )
#'       )
#'     ) |> run()
#'     coh$attributes$stats <- c("pre", "post")
#'     observeEvent(input$step_two_max, {
#'       coh$update_filter("copies", step_id = 2, range = c(6, input$step_two_max))
#'       run(coh, min_step_id = "2")
#'       .update_data_stats(coh$get_source(), step_id = "2", cohort = coh, session = session)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name updating-data-statistics
#' @seealso \link{source-gui-layer}
#' @export
.update_data_stats <- function(source, ...) {
  UseMethod(".update_data_stats", source)
}

#' @rdname updating-data-statistics
#' @export
.update_data_stats.default <- function(source, step_id, cohort, session, ...) {
  ns <- session$ns
  stats <- cohort$attributes$stats
  # Data statistics follow the `stats` setting. When stats are disabled the
  # stats are not read, so this is a no-op (avoids forcing a source scan).
  if (is.null(stats)) {
    return(invisible(NULL))
  }
  selector <- paste0("#", ns(paste0(step_id, "-stats")))

  # Read the parent (pre) snapshot stats, recomputing on demand when missing.
  # Every step's "pre" data is its parent's "post" snapshot, which always exists:
  # step 1's parent is the source, and steps 2+ are seeded from their parent at
  # construction / add_step (see Cohort$init_source / add_step). So recomputing
  # "pre" stats is safe in every mode (run_button, compute_stats = FALSE, freshly added
  # step) and lets step 1 show real stats before any run instead of the
  # "no data" placeholder. The placeholder is then reserved for its true meaning:
  # the parent step actually filtered out every row (previous == 0). NULL is
  # still handled defensively so `if (!NULL > 0)` cannot error with
  # "argument is of length zero".
  previous <- cohort$get_stats(step_id, state = "pre", .recalc_when_missing = TRUE)$n_rows
  if (is.null(previous) || !isTRUE(previous > 0)) {
    # Wrap in an element so the removeUI(" > *") cleanup can remove it on the
    # next update. A bare string is inserted as a text node, which the
    # child-element selector cannot match, leaving stale text behind (e.g. the
    # placeholder lingering next to freshly computed stats after a run).
    ui <- shiny::tags$span("No data selected in previous step.")
  } else {
    current <- cohort$get_stats(step_id, state = "post")$n_rows
    ui <- .pre_post_stats(current, previous, percent = TRUE, stats = stats)
  }
  shiny::removeUI(selector = paste0(selector, " > *"), multiple = TRUE, immediate = TRUE)
  shiny::insertUI(selector = selector, ui = ui, immediate = TRUE)
}

#' Action: refresh a step's data statistics display
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_update_data_stats <- function(cohort, changed_input, session) {

  print_state("update_data_stats", changed_input)
  input_state("update_data_stats", changed_input)
  .update_data_stats(cohort$get_source(), changed_input$step_id, cohort, session)
}

#' Clear every filter in a step back to its default
#' @param cohort The cohort object.
#' @param step_id Id of the step to clear.
#' @return Invisibly `NULL`; clears each filter for its side effect.
#' @noRd
reset_filters <- function(cohort, step_id) {

  filter_ids <- names(cohort$get_step(step_id)$filters)
  for (filter_id in filter_ids) {
    # question - can we optimize such expressions by succesively updating metadata in cohort?
    filter <- cohort$get_filter(step_id, filter_id)
    # todo if I remember correctly we don't handle here one case when reset is false (to check which one)
    cohort$clear_filter(step_id = step_id, filter_id = filter_id)
  }
}

#' Action: clear a step's filters and refresh its UI
#'
#' Optionally resets the step's filters, then re-runs the flow (immediate mode)
#' or refreshes inputs/plots and data stats (run-button mode).
#'
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_clear_step <- function(cohort, changed_input, session) {
  print_state("clear_step", changed_input)
  input_state("clear_step", changed_input)

  if (isTRUE(changed_input$reset)) {
    reset_filters(cohort, changed_input$step_id)
  }

  if (is_none(cohort$attributes$run_button)) {
    cohort$run_flow(min_step = changed_input$step_id)
  } else {
    ui_update_filters_loop(
      cohort, changed_input$step_id, reset = changed_input$reset,
      update = c("input", "plot"), session = session
    )
    action_update_data_stats(cohort, list(step_id = changed_input$step_id), session)
  }
}

#' Action: show a filter/field help description in a modal
#' @inheritParams action_update_filter
#' @return Invisibly; called for its side effects.
#' @noRd
action_show_help <- function(cohort, changed_input, session) {
  description <- do.call(cohort$show_help, changed_input)
  if(is.null(description)) return(invisible(FALSE))
  name <-if (is.null(changed_input$field)) {
    do.call(cohort$get_filter, changed_input)@name
  } else {
    changed_input$field
  }
  shiny::showModal(
    shiny::modalDialog(
      title = name,
      easyClose = TRUE,
      description
    )
  )
  return(invisible(TRUE))
}

# notes
# 2. Add method in shiny object, i.e.: shiny:::ShinySession$set("rm_inputs", "public", function(id) {...})
