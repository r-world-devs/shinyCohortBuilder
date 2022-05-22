extract_selected_range <- function(range, parent_range, reset) {

  if (reset || identical(range, NA) || !any(dplyr::between(range, parent_range[1], parent_range[2]))) {
    return(parent_range)
  }
  if (range[1] < parent_range[1]) {
    range[1] <- parent_range[1]
  }
  if (range[2] > parent_range[2]) {
    range[2] <- parent_range[2]
  }

  return(range)
}

freq_range <- function(freqs_table) {
  if (nrow(freqs_table) == 0) {
    return(NULL)
  }
  c(freqs_table$l_bound[1], rev(freqs_table$u_bound)[1])
}

freq_step <- function(freqs_table) {
  if (nrow(freqs_table) == 0) {
    return(1)
  }
  if (nrow(freqs_table) == 1) {
    return(1)
  }
  round(freqs_table$l_bound[2] - freqs_table$l_bound[1], 10)
}

range_input_defaults <- function(id, type = "range") {
  if (type == "range") {
    return(
      list(
        inputId = id,
        min = -1,
        max = -1,
        value = c(-1, -1),
        label = NULL
      )
    )
  }
  if (type == "date_range") {
    return(
      list(
        inputId = id,
        label = NULL
      )
    )
  }
  return(
    list(
      inputId = id,
      label = NULL
    )
  )
}

range_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  step_id <- filter$step_id
  filter_id <- filter$id

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      range_input_defaults(input_id, filter$type)
    )
  }

  parent_filter_stats <- cohort$get_cache(step_id, filter_id, state = "pre")$frequencies
  parent_range <- freq_range(parent_filter_stats)

  selected_range <- extract_selected_range(
    filter$get_params("range"),
    parent_range, reset
  )

  params <- list(
    inputId = input_id,
    min = parent_range[1],
    max = parent_range[2],
    value = selected_range,
    label = if (update) character(0) else NULL,
    width = "100%",
    ...
  )

  if (inherits(filter, "range")) {
    if (!is.null(filter$get_params("step"))) {
      params$step <- filter$get_params("step")
    } else {
      params$step <- freq_step(parent_filter_stats)
    }
  }
  if (inherits(filter, "date_range")) {
    params$start <- params$value[1]
    params$end <- params$value[2]
    params$value <- NULL
  }

  if (update) {
    params$width <- NULL
  }

  return(params)
}

plot_feedback_hist <- function(plot_data, n_missing, n_total) {

  choosen_color <- "#51a2e5"

  gg_object <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = level, y = count)) +
    ggplot2::geom_bar(
      fill = choosen_color,
      colour = choosen_color,
      alpha  = 0.5,
      na.rm  = TRUE,
      stat = "identity"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background =
        ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background  =
        ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.subtitle =
        ggplot2::element_text(
          color = "dimgray", size = 10, face = "plain")) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      x = NULL, y = NULL,
      subtitle = glue::glue(
        "missing: {format_number(n_missing)}",
        " / {format_number(n_total)} ",
        "({round(n_missing / n_total, 1)}%)"))

  gg_object
}

suff_id <- function(params_list, suffix) {
  params_list$inputId <- paste0(params_list$inputId, "-", suffix)
  return(params_list)
}

is_gui_type <- function(filter, type) {
  gui_input <- filter$get_params("gui_input")
  if (is.null(gui_input)) {
    return(TRUE)
  }
  type %in% filter$get_params("gui_input")
}

#' @rdname gui-filter-layer
#' @export
.gui_filter.range <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_params <- range_input_params(filter, input_id, cohort, ...)

      shiny::tagList(
        if (is_gui_type(filter, "slider")) {
          .cb_input(
            do.call(
              shiny::sliderInput,
              append(list(ticks = FALSE, round = -2), suff_id(input_params, "slider"))
            ),
            filter$input_param
          )
        },
        if (is_gui_type(filter, "numeric")) {
          .cb_input(
            do.call(
              shinyWidgets::numericRangeInput,
              suff_id(input_params, "numrange")
            ),
            filter$input_param
          )
        },
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
          shiny::renderPlot(bg = "transparent", height = 60, {
            if(empty) { # when no data in parent step
              return(
                ggplot2::ggplot()
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
      input_params <- append(
        list(session = session),
        range_input_params(filter, input_id, cohort, reset, TRUE, ...)
      )
      if (is_gui_type(filter, "slider")) {
        do.call(
          shiny::updateSliderInput,
          suff_id(input_params, "slider")
        )
      }
      if (is_gui_type(filter, "numeric")) {
        do.call(
          shinyWidgets::updateNumericRangeInput,
          suff_id(input_params, "numrange")[c("session", "inputId", "label", "value")]
        )
      }
      .update_keep_na_input(session, input_id, filter, cohort)
    },
    post_stats = FALSE,
    multi_input = length(filter$get_params("gui_input")) != 1
  )
}
