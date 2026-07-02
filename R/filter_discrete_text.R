#' Split a comma-separated discrete_text string into unique trimmed values
#'
#' Mirrors cohortBuilder's `split_discrete_text()`: whitespace around every value
#' is stripped (not just the first space) so `"a, b, c"` yields all three
#' values, and empty pieces are dropped.
#'
#' @param x A comma-separated string (or `NULL`/`NA`/`""`).
#' @return A character vector of unique, non-empty values.
#' @noRd
split_discrete_text_vals <- function(x) {
  if (is.null(x) || identical(x, NA) || identical(x, "")) {
    return(character(0))
  }
  pieces <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  unique(pieces[nzchar(pieces)])
}

#' Keep only selected discrete_text values present in the available set
#'
#' Returns all `original` values on reset/`NA`, passes `""` through, and
#' otherwise drops selected values not present in `original`.
#'
#' @param selected Comma-separated selected values (or `NA`/`""`).
#' @param original Comma-separated available values.
#' @param reset When `TRUE`, return all `original` values.
#' @return A comma-separated string of matching values.
#' @noRd
get_matching_vals <- function(selected, original, reset = FALSE) {

  if (reset || identical(selected, NA)) {
    return(original)
  }

  if (identical(selected, "")) {
    return(selected)
  }

  selected_vec <- split_discrete_text_vals(selected)
  original_vec <- split_discrete_text_vals(original)

  if (!all(selected_vec %in% original_vec)) {
    return(paste(intersect(selected_vec, original_vec), collapse = ","))
  }

  return(selected)
}

#' Count selected discrete_text values present in the available set
#' @param selected Comma-separated selected values (or `NA`).
#' @param original Comma-separated available values.
#' @return Number of selected values found in `original`.
#' @noRd
get_n_matching_vals <- function(selected, original) {

  original_vec <- split_discrete_text_vals(original)
  if (identical(selected, NA)) {
    return(length(original_vec))
  }
  selected_vec <- split_discrete_text_vals(selected)

  sum(selected_vec %in% original_vec)
}

#' Build text-area input params for a discrete_text filter
#'
#' Resolves the selected value against the parent's available choices, returning
#' an empty value when the parent step holds no data.
#'
#' @param filter A cohortBuilder filter object.
#' @param input_id Base input id.
#' @param cohort The cohort object.
#' @param reset When `TRUE`, select all available values.
#' @param update When `TRUE`, build params for an update (vs initial render).
#' @param ... Extra params forwarded to the input constructor.
#' @return A named list of input constructor params.
#' @noRd
discrete_text_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id

  if (!cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data")) {
    return(
      list(inputId = input_id, value = "", label = NULL)
    )
  }

  parent_choices <- cohort$get_stats(step_id, filter_id, state = "pre", name = "choices")
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
              rlang::inject(shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("[data-id=\"", input_params$inputId, "\""),
                !!!bs_data_attr("dismiss", "modal"),
                onclick = move_dialog_back_js, try_binding = FALSE
              )),
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
            !!!bs_data_attr("toggle", "modal"), !!!bs_data_attr("target", paste0("#", modal_dialog_id)),
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

            filter_stats <- cohort$get_stats(step_id, filter_id, state = "pre")
            n_total <- filter_stats$n_data

            n_selected <- get_n_matching_vals(filter@value, filter_stats$choices)
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
