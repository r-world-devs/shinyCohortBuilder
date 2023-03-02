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
#' \item{update_filter}{ Calls `shinyCohortBuilder:::gui_update_filter` that triggers filter arguments update.}
#' \item{add_step}{ Calls `shinyCohortBuilder:::gui_add_step` that triggers adding a new filtering step (based on configuration of the previous one).}
#' \item{rm_step}{ Calls `shinyCohortBuilder:::gui_rm_step` used to remove a selected filtering step.},
#' \item{clear_step}{ Calls `shinyCohortBuilder:::gui_clear_step` used to clear filters configuration in selected step.}
#' \item{update_step}{ Calls `shinyCohortBuilder:::gui_update_step` used to update filters and feedback plots for the specific filter GUI panel.}
#' \item{update_data_stats}{ Calls `shinyCohortBuilder:::gui_update_data_stats` that is called to update data statistics. }
#' \item{show_repro_code}{ Calls `shinyCohortBuilder:::gui_show_repro_code` that is used to show reproducible code. }
#' \item{run_step}{ Calls `shinyCohortBuilder:::gui_run_step` used to trigger specific step data calculation. }
#' \item{show_state}{ Calls `shinyCohortBuilder:::gui_show_state` that is used to show filtering panel state json. }
#' \item{input_state}{ Calls `shinyCohortBuilder:::gui_input_state` that is used to generate modal in which filtering panel state can be provided (as json). }
#' \item{restore_state}{ Calls `shinyCohortBuilder:::gui_restore_state` used for restoring filtering panel state based on provided json. }
#' \item{show_attrition}{ Calls `shinyCohortBuilder:::gui_show_attrition` a method used to show attrition data plot(s).}
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

sf_id <- function(step_id, filter_id) {
  paste(step_id, filter_id, sep = "-")
}

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
      params %>% purrr::imap(
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

gui_update_filter_class <- function(step_id, filter_id, show, class, session) {
  session$sendCustomMessage(
    "update_filter_class",
    list(
      step_id = step_id, filter_id = filter_id,
      show = show, ns_prefix = session$ns(""), class = class
    )
  )
}

update_filter_gui <- function(cohort, step_id, filter_id, update, reset, session) {
  filter <- cohort$get_filter(step_id, filter_id)
  updated_input <- FALSE
  updated_plot <- FALSE

  if (("post_input" %in% update) && !identical(filter$gui$post_stats, FALSE)) {
    update <- c(update, "input")
  }
  if (("multi_input" %in% update) && filter$gui$multi_input) {
    update <- c(update, "input")
  }

  if ("input" %in% update) {
    updated_input <- TRUE

    filter$gui$update(
      session,
      sf_id(step_id, filter_id),
      cohort,
      reset = reset
    )
  }
  if ("plot" %in% update) {
    show <- TRUE
    if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
      show <- FALSE
    }
    gui_update_filter_class(step_id, filter_id, show, "cb_no_data", session)
    show_feedback <- if_null_default(
      filter$get_params("feedback"),
      cohort$attributes$feedback
    )
    if (show_feedback) {
      updated_plot <- TRUE
      gui_update_plot(step_id, filter_id, cohort, session) # todo optmize to not extract filter again inside plot
    }
  }
  input_state(
    "update_filter_gui",
    list(
      step_id = step_id, filter_id = filter_id, reset = reset,
      updated_plot = updated_plot, updated_input = updated_input
    )
  )
  print_state(
    "update_filter_gui",
    list(
      step_id = step_id, filter_id = filter_id, reset = reset,
      updated_plot = updated_plot, updated_input = updated_input
    )
  )
}

gui_update_filters_loop <- function(cohort, step_id, reset, update, exclude = character(0), session) {
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
    update_filter_gui(cohort, step_id, filter_id, update, reset, session)
  }
}

empty_plot <- function() {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  grDevices::png(tempfile(fileext = ".png"), height = 1)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  grDevices::dev.off()
}

gui_update_plot <- function(step_id, filter_id, cohort, session) {
  ns <- session$ns

  print_state("update_plot", list(step_id = step_id, filter_id = filter_id))
  input_state("update_plot", list(step_id = step_id, filter_id = filter_id))

  filter <- cohort$get_filter(step_id, filter_id)
  no_data <- cohort$get_cache(step_id, filter_id, state = "pre")$n_data == 0
  feedback <- filter$gui$feedback(sf_id(step_id, filter_id), cohort, no_data)
  session$output[[feedback$plot_id]] <- feedback$render_fun
}

update_next_step <- function(cohort, step_id, reset, session) {
  next_step_id <- as.integer(step_id) + 1
  if (next_step_id <= length(cohort$get_step())) {
    if (reset) {
      reset_filters(cohort, as.character(next_step_id))
    }
    gui_update_step(
      cohort,
      list(
        step_id = as.character(next_step_id), run_flow = TRUE,
        reset = reset, update = c("input", "plot")
      ),
      session
    )
  }
}

input_val_handler <- function(val) {
  if (is.list(val)) {
    if (is.null(names(val))) {
      return(unlist(val, recursive = TRUE))
    }
    return(
      val %>% purrr::map(unlist) %>%
        purrr::map_if(~all(.x %in% c("TRUE", "FALSE", "NA")), as.logical)
    )
  } else {
    return(val)
  }
}

convert_input_value <- function(changed_input, step_id, filter_id, cohort, update_active) {
  # todo handle case when no value parameter defined (some filters can work like that)

  changed_input[changed_input$input_name] <- list(input_val_handler(changed_input$input_value))
  is_date_filter <- inherits(cohort$get_filter(step_id, filter_id), "date_range")
  update_keep_na <- changed_input$input_name == "keep_na"
  if (is_date_filter && !update_active && !update_keep_na) {
    changed_input[[changed_input$input_name]] <- as.Date(changed_input[[changed_input$input_name]])
  }
  changed_input$input_name <- NULL
  changed_input$input_value <- NULL

  return(changed_input)
}

gui_update_filter <- function(cohort, changed_input, session) {

  run_on_request <- !is_none(cohort$attributes$run_button)
  update_active <- changed_input$input_name == "active"

  step_id <- changed_input$step_id
  filter_id <- changed_input$filter_id
  data_filter <- cohort$get_filter(step_id, filter_id)

  force_render <- getOption("scb_render_all", default = FALSE)

  print_state("update_filter", changed_input)
  input_state("update_filter", changed_input)

  if (run_on_request) {
    trigger_pending_state(step_id, "add", session)
  }

  changed_input <- convert_input_value(changed_input, step_id, filter_id, cohort, update_active)
  do.call(
    cohort$update_filter,
    changed_input
  )
  if (!run_on_request) {
    cohort$run_step(step_id)
  }

  run_update <- TRUE
  if (!force_render && !is.null(changed_input$active)) {
    run_update <- !insert_filter(step_id, filter_id, cohort, session)
  }
  filter_stats <- if_null_default(
    data_filter$get_params("stats"),
    cohort$attributes$stats
  )
  post_stats_visible <- "post" %in% filter_stats
  if (run_update) {
    update <- c("plot", "multi_input")
    if (!run_on_request && post_stats_visible) {
      update <- c(update, "post_input")
    }
    update_filter_gui(cohort, step_id, filter_id, update, FALSE, session)
  }

  if (!run_on_request && ("post" %in% cohort$attributes$stats)) {
    update <- "post_input"
    gui_update_filters_loop(cohort, step_id, FALSE, update, exclude = filter_id, session)
  }

  if (update_active) {
    gui_update_filter_class(step_id, filter_id, changed_input$active, "hidden-input", session)
  }

  if (is_none(cohort$attributes$run_button)) {
    gui_update_data_stats(cohort, list(step_id = step_id), session)
    update_next_step(cohort, step_id, FALSE, session)
  }
}

insert_filter <- function(step_id, filter_id, cohort, session) {
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

gui_rm_step <- function(cohort, changed_input, session) {
  ns <- session$ns

  # todo make sure to diasble delete button when source is updated
  print_state("rm_step", changed_input)
  input_state("rm_step", changed_input)
  cohort$remove_step(run_flow = TRUE)

  clear_step_data(changed_input$step_id, session)
  shiny::removeUI(glue::glue("#{ns(changed_input$step_id)}"), session = session, immediate = TRUE)
  session$sendCustomMessage("post_rm_step_action", list(id = changed_input$step_id, ns_prefix = ns("")))
}

gui_run_step <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("run_step", changed_input)
  input_state("run_step", changed_input)

  gui_update_step(cohort, changed_input, session)
  trigger_pending_state(changed_input$step_id, "remove", session)
}

gui_show_state <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_state", changed_input)
  input_state("show_state", changed_input)

  shiny::showModal(shiny::modalDialog(
    size = "l",
    title = "Cohort state",
    shiny::tags$code(
      cohort$get_state(json = TRUE) %>%
        shiny::HTML()
    )
  ))
}

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

gui_input_state <- function(cohort, changed_input, session) {
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
    footer = shiny::tagList(
      shiny::modalButton("Confirm") %>%
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

gui_restore_state <- function(cohort, changed_input, session) {
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

gui_add_step <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("add_step", changed_input)
  input_state("add_step", changed_input)
  available_filters <- cohort$attributes$available_filters

  if (length(cohort$get_step()) == 0 && length(available_filters) > 0) {
    cohort$copy_step(
      filters = available_filters,
      run_flow = TRUE
    )
  } else {
    cohort$copy_step(run_flow = TRUE)
  }

  last_step_id <- cohort$last_step_id()

  session$sendCustomMessage("pre_add_step_action", list(id = last_step_id, ns_prefix = ns("")))

  render_step(
    cohort,
    last_step_id,
    active = TRUE,
    allow_rm = TRUE,
    session$input, session$output, session
  )
}

gui_add_step_configured <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("gui_add_step_configured", changed_input)
  input_state("gui_add_step_configured", changed_input)

  chosed_filters <- session$input[["configure_step"]]
  available_filters <- cohort$attributes$available_filters

  if (length(available_filters) == 0) {
    warning_nl("`available_filters` was not defined, configure step will not be working. Cloning last step.")
    return(gui_add_step(cohort, changed_input, session))
  }

  filters <- available_filters %>%
    purrr::keep(function(x) {x$id %in% chosed_filters})

  cohort$copy_step(
    filters = filters,
    run_flow = TRUE
  )

  last_step_id <- cohort$last_step_id()
  session$sendCustomMessage("pre_add_step_action", list(id = last_step_id, ns_prefix = ns("")))

  render_step(
    cohort,
    last_step_id,
    active = TRUE,
    allow_rm = TRUE,
    session$input, session$output, session
  )
}

gui_show_step_filter_modal <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("add_step_modal", changed_input)
  input_state("add_step_modal", changed_input)

  available_filters <- cohort$attributes$available_filters

  if (length(available_filters) == 0) {
    warning_nl("`available_filters` was not defined, configure step will not be working. Cloning last step.")
    return(gui_add_step(cohort, changed_input, session))
  }

  choices <- .available_filters_choices(cohort$get_source(), cohort)

  shiny::showModal(
    shiny::modalDialog(
      shinyWidgets::virtualSelectInput(
        ns("configure_step"),
        label = "Chose filters",
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
        shinyGizmo::valueButton(
          inputId = ns("add_step_configured"),
          label = "Accept",
          selector = paste0("[data-id=\"", ns("configure_step"), "\"]"),
          onclick = .trigger_action_js("add_step_configure", ns = ns),
          `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
          disabled = NA
        ),
        shiny::modalButton("Dismiss")
      ),
      title = "Configure new step",
      size = "m",
      easyClose = FALSE
    )
  )

}

trigger_pending_state <- function(step_id, action, session) {
  session$sendCustomMessage(
    "update_class",
    list(
      step_id = step_id, class = "pending", action = action,
      disable = ".cb_run_step", ns_prefix = session$ns("")
    )
  )
}

# idea
# when updating filter run only `run_step` for current step
# then in gui_update_step:
# 1. update gui filters first
# 2. take parameters from gui filters and update the ones in cohort
# 3. call run_step
# this should allow to have fron-back always up to date
# probably impossible due to cache being called before rendering
gui_update_step <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("update_step", changed_input)
  input_state("update_step", changed_input)
  filter_ids <- names(cohort$get_step(changed_input$step_id)$filters)

  # todo make sure this is needed
  if (changed_input$run_flow) {
    cohort$run_step(step_id = changed_input$step_id)
  }

  gui_update_filters_loop(
    cohort, changed_input$step_id, changed_input$reset,
    changed_input$update, session = session
  )
  gui_update_data_stats(cohort, list(step_id = changed_input$step_id), session)

  if (changed_input$run_flow) {
    update_next_step(cohort, changed_input$step_id, FALSE, session)
  }
}

add_trailing_space <- function(string) {
  n_spaces <- nchar(regmatches(string, regexpr("^\\s+", string)))
  if (!length(n_spaces)) {
    return(string)
  }
  gsub("^\\s+", paste(rep("&nbsp", n_spaces), collapse = ""), string)
}

gui_show_repro_code <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_code", changed_input)
  input_state("show_code", changed_input)

  shiny::showModal(shiny::modalDialog(
    size = "xl",
    title = "Reproducible code",
    shiny::tags$code(
      class = "hl background",
      cohort$get_code(width = I(120), output = FALSE)$text.tidy %>%
        highr::hi_html() %>%
        purrr::map_chr(add_trailing_space) %>%
        paste(collapse = "</br>") %>%
        shiny::HTML()
    )
  ))
}

#' Generate output of attrition plot
#'
#' @description
#' The method should return list of two object:
#' \itemize{
#'   \item{render}{ Rendering expression of attrition output.}
#'   \item{output}{ Output expression related to rendering (with id equal to `id` parameter).}
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
#'   ) %>% run()
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
#'   ) %>% run()
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

get_bs <- function () {
  theme <- shiny::getCurrentTheme()
  if (bslib::is_bs_theme(theme)) {
    bslib::theme_version(theme)
  } else {
    "3"
  }
}

bump_tab_version <- function(li_item, selected) {
  li_item$attribs$class <- "nav-item"
  link_class <- "nav-link"
  if (li_item$children[[1]]$attribs$`data-value` == selected) {
    link_class <- paste(link_class, "active")
  }
  li_item$children[[1]]$attribs$class <- link_class
  return(li_item)
}

navs <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"),
                 header = NULL,  footer = NULL) {
  tabset <- shiny::tabsetPanel(
    ..., id = id, selected = selected, type = type, header = header, footer = footer
  )
  if (get_bs() == "3") {
    if (is.null(selected)) {
      # todo write it as it should be
      selected <- tabset$children[[1]]$children[[1]]$children[[1]]$attribs$`data-value`
    }
    tabset$children[[1]]$children <- tabset$children[[1]]$children %>%
      purrr::modify(bump_tab_version, selected = selected)
  }
  return(tabset)
}

gui_show_attrition <- function(cohort, changed_input, session) {
  ns <- session$ns

  print_state("show_attrition", changed_input)
  input_state("show_attrition", changed_input)

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
    ui
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
#' When rendering the output, a good practice is to use cached data statistics available with
#' `cohort$get_cache(step_id)`.
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
#'     ) %>% run()
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

  session$output[[paste0(step_id, "-stats")]] <- shiny::renderUI({
    previous <- cohort$get_cache(step_id, state = "pre")$n_rows
    if (!previous > 0) {
      return("No data selected in previous step.")
    }
    current <- cohort$get_cache(step_id, state = "post")$n_rows
    .pre_post_stats(current, previous, percent = TRUE, stats = stats)
  })
}

gui_update_data_stats <- function(cohort, changed_input, session) {

  print_state("update_data_stats", changed_input)
  input_state("update_data_stats", changed_input)
  .update_data_stats(cohort$get_source(), changed_input$step_id, cohort, session)
}

reset_filters <- function(cohort, step_id) {

  filter_ids <- names(cohort$get_step(step_id)$filters)
  for (filter_id in filter_ids) {
    # question - can we optimize such expressions by succesively updating metadata in cohort?
    filter <- cohort$get_filter(step_id, filter_id)
    # todo if I remember correctly we don't handle here one case when reset is false (to check which one)
    cohort$clear_filter(step_id = step_id, filter_id = filter_id)
  }
}

gui_clear_step <- function(cohort, changed_input, session) {
  print_state("clear_step", changed_input)
  input_state("clear_step", changed_input)

  reset_filters(cohort, changed_input$step_id)

  if (!is_none(cohort$attributes$run_button)) {
    trigger_pending_state(changed_input$step_id, "add", session)
    changed_input$run_flow <- FALSE
  }
  changed_input$update <- c("input", "plot")
  gui_update_step(cohort, changed_input, session)
}

gui_show_help <- function(cohort, changed_input, session) {
  description <- do.call(cohort$show_help, changed_input)
  if(is.null(description)) return(invisible(FALSE))
  name <-if (is.null(changed_input$field)) {
    do.call(cohort$get_filter, changed_input)$name
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
