get_matching_vals <- function(selected, original, reset = FALSE) {

  if (reset || identical(selected, NA) || identical(selected, NULL)) {
    return(list())
  }

  return(selected)
}

filter_from_stats <- function(variable, classes, stats) {
  filter_class <- classes[[variable]][1]
  list(
    id = variable,
    field = variable,
    type = shinyQueryBuilder:::type_mappers[[filter_class]]$type,
    input = shinyQueryBuilder:::type_mappers[[filter_class]]$input,
    values = stats[[variable]],
    placeholder = glue::glue("Choose {variable}"),
    operators = shinyQueryBuilder:::type_operators(filter_class)
  )
}

query_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {

  step_id <- filter$step_id
  filter_id <- filter$id

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id, value = list())
    )
  }

  parent_choices <- cohort$get_cache(step_id, filter_id, state = "pre")$choices
  choice_classes <- cohort$get_cache(step_id, filter_id, state = "pre")$classes
  filters <- filter$get_params("variables") %>%
    purrr::map(filter_from_stats, classes = choice_classes, stats = parent_choices)
  selected_value <- get_matching_vals(
    filter$get_params("value"),
    filters,
    reset
  )

  params <- list(
    inputId = input_id,
    rules = selected_value,
    filters = filters,
    ...
  )

  return(params)
}

#' @rdname gui-filter-layer
#' @export
.gui_filter.query <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_params <- query_input_params(filter, input_id, cohort)
      input_params$inputId <- paste0(input_id, "_selected")

      # input_params$width <- "100%"
      # parent <- input_params$all
      # input_params$all <- NULL
      # input_params$readonly <- FALSE
      # input_params$inputId <- paste0(input_id, "_selected")

      shiny::tagList(
        shinyGizmo::modalDialogUI(
          paste0(input_id, "modal_in"),
          do.call(shinyQueryBuilder::queryBuilderInput, input_params),
          backdrop = FALSE,
          size = "l",
          footer = shiny::tagList(
            .cb_input(
              shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("#", input_params$inputId),
                `data-dismiss` = "modal", `data-bs-dismiss` = "modal"
              ),
              filter$input_param,
              style = "display: inline-block;"
            ),
            shiny::modalButton("Dismiss")
          ),
          button = button(
            "Set rules",
            icon = shiny::icon("keyboard"), style = "width: 100%; margin-top: 0.5em; margin-bottom: 0.5em;",
            `data-toggle` = "modal", `data-target` = paste0("#", paste0(input_id, "modal_in"))
          )
        ),
        .cb_input(
          .keep_na_input(input_id, filter, cohort),
          "keep_na"
        )
      )
    },
    feedback = function(input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot") ,
        output_fun = ggiraph::girafeOutput,
        render_fun = if (!is.null(empty)) {
          ggiraph::renderGirafe({
            if(empty) { # when no data in parent step
              return(
                ggiraph::girafe(
                  ggobj      = ggplot2::ggplot(),
                  width_svg  = 10,
                  height_svg = 0.1
                )
              )
            }
            step_id <- filter$step_id
            filter_id <- filter$id
            filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
            orig_values <- filter$get_params("values")
            if (is.null(orig_values)) {
              orig_values <- filter_cache$choices %>%
                purrr::map(names)
            } else {
              orig_values <- orig_values %>%
                purrr::map(~as.character(unlist(.)))
            }
            filter_value <- purrr::map2(
              stats::setNames(orig_values[names(filter_cache$choices)], names(filter_cache$choices)),
              filter_cache$choices,
              ~extract_selected_value(.x, .y, FALSE)
            )

            plot_data <- filter_cache$choices %>%
              purrr::imap(function(x, y) {x[unlist(filter_value[y])]}) %>%
              grouped_list_to_df() %>%
              dplyr::bind_rows()
            n_missing <- data.frame(
              variable = names(filter_cache$n_missing),
              state = "(missing)",
              value = unlist(filter_cache$n_missing)
            ) %>%
              dplyr::filter(variable %in% plot_data$variable)
            if (identical(filter$get_params("keep_na"), FALSE)) {
              n_missing$value <- 0
            }

            plot_feedback_multi_bar(plot_data, n_missing)
          })
        }
      )
    },
    server = function(input_id, input, output, session, cohort) {},
    update = function(session, input_id, cohort, reset = FALSE, ...) {
      update_params <- query_input_params(filter, input_id, cohort, reset, TRUE, ...)
      update_params$inputId <- paste0(input_id, "_selected")

      do.call(
        shinyQueryBuilder::updateQueryBuilderInput,
        append(
          list(session = session),
          update_params
        )
      )
    },
    post_stats = FALSE,
    multi_input = FALSE
  )
}
