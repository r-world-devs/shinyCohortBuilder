#' Clamp a selected datetime range to the parent's available range
#'
#' Coerces character/POSIXct input, replaces out-of-bounds or missing endpoints
#' with the parent bounds, and falls back to the full parent range on reset or
#' when the selection lies entirely outside it.
#'
#' @param range The selected `c(from, to)` datetime range (or empty/sentinel).
#' @param parent_range The parent step's available `c(min, max)` range.
#' @param reset When `TRUE`, ignore `range` and return `parent_range`.
#' @return A clamped `c(from, to)` datetime range.
#' @noRd
extract_selected_datetime_range <- function(range, parent_range, reset) {
  if (identical(range, c(Inf, -Inf)) || length(range) == 0) {
    return(range)
  }

  if (inherits(range, "character") || inherits(range, "POSIXct")) {
    if (length(range) == 1) range <- c(range, Inf)

    range <- as.POSIXct(range, origin = "1970-01-01 UTC")
    parent_range <- as.POSIXct(parent_range, origin = "1970-01-01 UTC")
  }

  if (reset || identical(range, NA) || !any(dplyr::between(range, parent_range[1], parent_range[2]))) {
    return(parent_range)
  }
  if (anyNA(range[1]) || range[1] < parent_range[1]) {
    range[1] <- parent_range[1]
  }
  if (anyNA(range[2]) || range[2] > parent_range[2]) {
    range[2] <- parent_range[2]
  }

  return(range)
}

S7::method(.gui_filter, cohortBuilder::CbFilterDatetimeRange) <- function(object, ...) {
  list(
    input = function(filter, input_id, cohort) {
      input_params <- range_input_params(filter, input_id, cohort, ...)
      shiny::tagList(
        if (is_gui_type(filter, "datetimepicker")) {
          input_params$minDate <- input_params$min
          input_params$min <- NULL
          input_params$maxDate <- input_params$max
          input_params$max <- NULL
          input_params$step <- NULL
          .cb_input(
            do.call(
              shinyWidgets::airDatepickerInput,
              modify_list(
                list(
                  range = TRUE, timepicker = TRUE, update_on = "close",
                  autoClose = TRUE, addon = "none"
                ),
                suff_id(input_params, "datetimepicker")
              )
            ),
            filter@private$input_param
          )
        } else if (is_gui_type(filter, "slider")) {
          .cb_input(
            do.call(
              shiny::sliderInput,
              modify_list(
                list(ticks = FALSE, round = 1),
                suff_id(input_params, "slider")
              )
            ),
            filter@private$input_param
          )
        },
        .cb_input(
          .keep_na_input(input_id, filter, cohort),
          "keep_na"
        )
      )
    },

    feedback = function(filter, input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot"),
        output_fun = shiny::uiOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderUI({
            if (empty || is.null(filter@range)) {
              return(shiny::div(class = "cb_fb_bar"))
            }
            step_id <- filter@step_id
            filter_id <- filter@id

            filter_stats <- cohort$get_stats(step_id, filter_id, state = "pre")

            filter_range <- extract_selected_datetime_range(
              filter@range,
              freq_range(filter_stats$frequencies),
              FALSE
            )

            plot_data <- filter_stats$frequencies |>
              dplyr::mutate(
                count = ifelse(l_bound >= filter_range[1] & l_bound <= filter_range[2], count, 0)
              )
            n_missing <- filter_stats$n_missing
            n_total <- filter_stats$n_data
            if (identical(filter@keep_na, FALSE)) {
              n_missing <- 0
            }

            html_feedback_hist(plot_data, n_missing, n_total)
          })
        }
      )
    },
    server = function(filter, input_id, input, output, session, cohort) {},
    update = function(filter, session, input_id, cohort, reset = FALSE, ...) {
      input_params <- append(
        list(session = session),
        range_input_params(filter, input_id, cohort, reset, TRUE, ...)
      )
      if (is_gui_type(filter, "datetimepicker")) {
        input_params$min <- NULL
        input_params$max <- NULL
        input_params$step <- NULL
        do.call(
          shinyWidgets::updateAirDateInput,
          suff_id(input_params, "slider")
        )
      } else if (is_gui_type(filter, "slider")) {
        do.call(
          shiny::updateSliderInput,
          suff_id(input_params, "slider")
        )
      }
      .update_keep_na_input(session, input_id, filter, cohort)
    },

    post_stats = FALSE,
    multi_input = FALSE
  )
}
