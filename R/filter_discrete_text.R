get_matching_vals <- function(selected, original, reset = FALSE) {

  if (reset || identical(selected, NA)) {
    return(original)
  }

  if (identical(selected, "")) {
    return(selected)
  }

  selected_vec <- unique(strsplit(sub(" ", "", selected, fixed = TRUE), ",", fixed = TRUE)[[1]])
  original_vec <- unique(strsplit(sub(" ", "", original, fixed = TRUE), ",", fixed = TRUE)[[1]])

  if (!all(selected_vec %in% original_vec)) {
    return(paste(intersect(selected_vec, original_vec), collapse = ","))
  }

  return(selected)
}

get_n_matching_vals <- function(selected, original) {

  original_vec <- unique(strsplit(sub(" ", "", original, fixed = TRUE), ",", fixed = TRUE)[[1]])
  if (identical(selected, NA)) {
    return(length(original_vec))
  }
  selected_vec <- unique(strsplit(sub(" ", "", selected, fixed = TRUE), ",", fixed = TRUE)[[1]])

  sum(selected_vec %in% original_vec)
}

discrete_text_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id, value = "", label = NULL)
    )
  }

  parent_choices <- cohort$get_cache(step_id, filter_id, state = "pre")$choices
  selected_value <- get_matching_vals(
    filter@value,
    parent_choices,
    reset
  )

  params <- list(
    inputId = input_id,
    value = selected_value,
    label = "Place values separated by commas here:",
    all = parent_choices,
    ...
  )

  if (update) {
    params$label <- NULL
  }

  return(params)
}


S7::method(.gui_filter, cohortBuilder::CbFilterDiscreteText) <- function(object, ...) {
  list(
    input = function(filter, input_id, cohort) {
      input_params <- modify_list(
        list(
          all = NULL, readonly = FALSE, width = "100%",
          inputId = paste0(input_id, "_selected")
        ),
        discrete_text_input_params(filter, input_id, cohort, ...)
      )
      parent <- input_params$all
      modal_dialog_id <- paste0(input_id, "modal_in")
      # fixes overlapping modal dialog by backdrop
      move_dialog_to_body_js <- paste0("$('#", modal_dialog_id, "').appendTo('body');")
      move_dialog_back_js <- paste0("$('#", modal_dialog_id, "').appendTo('#", input_id, " .cb_inputs');")

      shiny::tagList(
        shinyGizmo::modalDialogUI(
          modal_dialog_id,
          do.call(shinyGizmo::textArea, input_params),
          shinyGizmo::textArea(paste0(input_id, "show_all"), parent, "Possible values", readonly = TRUE),
          backdrop = TRUE,
          size = "l",
          footer = shiny::tagList(
            .cb_input(
              shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("[data-id=\"", input_params$inputId, "\""),
                `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
                onclick = move_dialog_back_js, try_binding = FALSE
              ),
              filter@private$input_param,
              style = "display: inline-block;"
            ),
            shiny::modalButton("Dismiss") |>
              htmltools::tagAppendAttributes(
                onclick = move_dialog_back_js
              )
          ),
          button = button(
            getOption("scb_icons", scb_labels)$filter_discrete_text_bttn_label,
            icon = getOption("scb_icons", scb_icons)$filter_discrete_text_bttn_icon,
            class = "btn-sm scb-input-button",
            `data-toggle` = "modal", `data-target` = paste0("#", modal_dialog_id),
            `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", modal_dialog_id),
            onclick = move_dialog_to_body_js
          )
        )
      )
    },
    feedback = function(filter, input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot"),
        output_fun = shiny::uiOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderUI({
            if (empty) {
              return(shiny::div(class = "cb_fb_bar"))
            }
            step_id <- filter@step_id
            filter_id <- filter@id

            filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
            n_total <- filter_cache$n_data

            n_selected <- get_n_matching_vals(filter@value, filter_cache$choices)
            plot_data <- c("selected" = n_selected, "not_selected" = n_total - n_selected)

            html_feedback_text_bar(plot_data)
          })
        }
      )
    },
    server = function(filter, input_id, input, output, session, cohort) {},
    update = function(filter, session, input_id, cohort, reset = FALSE, ...) {
      input_fun <- shinyGizmo::updateTextArea
      update_params <- discrete_text_input_params(filter, input_id, cohort, reset, TRUE, ...)
      parent <- update_params$all
      update_params$all <- NULL
      update_params$inputId <- paste0(input_id, "_selected")

      do.call(
        input_fun,
        append(
          list(session = session),
          update_params
        )
      )
      input_fun(session, paste0(input_id, "show_all"), value = parent)
    },
    post_stats = FALSE,
    multi_input = FALSE
  )
}
