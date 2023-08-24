#' @rdname gui-filter-layer
#' @export
.gui_filter.date_range <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      shiny::tagList(
        .cb_input(
          do.call(
            shiny::dateRangeInput,
            modify_list(
              list(weekstart = 1, startview = "decade"),
              range_input_params(filter, input_id, cohort, ...)
            )
          ),
          filter$input_param
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
        output_fun = shiny::plotOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderPlot(bg = "transparent", height = 100, {
            if(empty) { # when no data in parent step
              return(
                empty_plot()
              )
            }
            step_id <- filter$step_id
            filter_id <- filter$id

            filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
            filter_range <- extract_selected_range(
              filter$get_params("range"),
              freq_range(filter_cache$frequencies),
              FALSE
            )

            plot_data <- filter_cache$frequencies %>%
              dplyr::mutate(# we take l_bound to limit upper cause last break have l_bound == u_bound
                count = ifelse(l_bound >= filter_range[1] & l_bound <= filter_range[2], count, 0)
              )

            if (!is.null(filter$get_params("n_bins"))) {
              intervals <- seq.Date(plot_data$l_bound[1], rev(plot_data$u_bound)[1], length.out = filter$get_params("n_bins"))
              plot_data <- plot_data %>%
                dplyr::mutate(level = findInterval(l_bound, intervals)) %>%
                dplyr::group_by(level) %>%
                dplyr::summarise(count = sum(count))
            }

            # todo possibly add modifier to lower number of bars
            n_missing <- filter_cache$n_missing
            n_total <- filter_cache$n_data
            if (identical(filter$get_params("keep_na"), FALSE)) {
              n_missing <- 0
            }

            plot_feedback_hist(plot_data, n_missing, n_total)
          })
        }
      )
    },
    server = function(input_id, input, output, session, cohort) {},
    update = function(session, input_id, cohort, reset = FALSE, ...) {
      do.call(
        shiny::updateDateRangeInput,
        append(
          list(session = session),
          range_input_params(filter, input_id, cohort, reset, TRUE, ...)
        )
      )
      .update_keep_na_input(session, input_id, filter, cohort)
    },
    post_stats = FALSE,
    multi_input = FALSE
  )
}
