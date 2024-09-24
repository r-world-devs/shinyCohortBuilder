extend_stats <- function(current, parent, inherit_parent = character(0)) {
  missing_stats <- setdiff(names(parent), names(current))
  for (missing_stat in missing_stats) {
    current[[missing_stat]] <- 0
    if (missing_stat %in% inherit_parent) {
      current[[missing_stat]] <- parent[[missing_stat]]
    }
  }
  current[names(parent)]
}

extract_selected_value <- function(value, parent_filter_stats, reset) {

  if (reset || identical(value, NA)) {
    return(names(parent_filter_stats))
  }
  if (is.null(value)) {
    return(value)
  }
  if (!all(value %in% names(parent_filter_stats))) {
    return(intersect(value, names(parent_filter_stats)))
  }
  return(value)
}

choice_name <- function(name, parent_stat, current_stat, stats) {
  .pre_post_stats(current_stat, parent_stat, name, brackets = TRUE, stats = stats)
}

#' @rdname pre_post_stats
#' @export
.pre_post_stats_text <- function(current, previous, name, brackets = TRUE,
                                 percent = FALSE, stats = c("pre", "post")) {
  glue::glue(
    "<span>",
    "{name}{open_bracket}{post_stat}{slash}{pre_stat}{close_bracket}",
    "{percent_open_bracket}{percent}{percent_close_bracket}",
    "</span>",
    .envir = list(
      name = empty_if_false(!missing(name), paste0(name, " "), FALSE, ""),
      open_bracket = empty_if_false(brackets && any(stats %in% c("pre", "post")), "(", FALSE, ""),
      post_stat = empty_if_false(
        "post" %in% stats,
        glue::glue("<span class = 'cb_delayed'>{current}</span>"),
        FALSE, ""
      ),
      slash = empty_if_false(length(stats) == 2, " / ", FALSE, ""),
      pre_stat = empty_if_false("pre" %in% stats, previous, FALSE, ""),
      close_bracket = empty_if_false(brackets && any(stats %in% c("pre", "post")), ")", FALSE, ""),
      percent_open_bracket = empty_if_false(percent && length(stats) == 2, " (", FALSE, ""),
      percent = empty_if_false(
        percent && length(stats) == 2,
        glue::glue("<span class = 'cb_delayed'>{round(100 * current / previous, 0)}%</span>"),
        FALSE, ""
      ),
      percent_close_bracket = empty_if_false(percent && length(stats) == 2, ")", FALSE, "")
    )
  )
}

is_vs <- function(filter) {
  !is.null(filter$get_params("gui_input")) && filter$get_params("gui_input") == "vs"
}

#' Generate NA's filter selection GUI input
#'
#' @description
#' When used within filter's GUI input method, the component is responsible for
#' updating `keep_na` filter parameter.
#'
#' Use `.update_keep_na_input` inside filter's GUI update method to update the
#' output based on the filter state.
#'
#' @examples
#' library(magrittr)
#' library(cohortBuilder)
#'
#' librarian_source <- set_source(as.tblist(librarian))
#' coh <- cohort(
#'   librarian_source,
#'   filter(
#'     "range", id = "copies", name = "Copies", dataset = "books",
#'     variable = "copies", range = c(5, 12)
#'   )
#' ) %>% run()
#' .keep_na_input("keep_na", coh$get_filter("1", "copies"), coh)
#'
#' @param input_id Id of the keep na input.
#' @param filter Filter object.
#' @param cohort Cohort object.
#' @param session Shiny session object.
#' @param msg_fun Function taking number of missing values as an argument and
#'   returning missing values label.
#'
#' @return Nested list of `shiny.tag` objects storing html structure of the input,
#' or no value in case of usage 'update' method.
#' @name keep_na_input
#' @export
.keep_na_input <- function(input_id, filter, cohort,
                           msg_fun = function(x) glue::glue("Keep missing values ({x})")) {

  filter_id <- filter$id
  step_id <- filter$step_id
  na_message <- cohort$get_cache(step_id, filter_id, state = "pre")$n_missing %>%
    msg_fun()

  shiny::tagList(
    shiny::checkboxInput(
      paste0(input_id, "-keep_na"),
      label = na_message,
      filter$get_params("keep_na")
    ) %>%
      shiny::tagAppendAttributes(class = "cb_na_input")
  )
}

#' @rdname keep_na_input
#' @export
.update_keep_na_input <- function(session, input_id, filter, cohort,
                                  msg_fun = function(x) glue::glue("Keep missing values ({x})")) {

  filter_id <- filter$id
  step_id <- filter$step_id
  na_message <- cohort$get_cache(step_id, filter_id, state = "pre")$n_missing %>%
    msg_fun()
  shiny::updateCheckboxInput(
    session,
    inputId = paste0(input_id, "-keep_na"),
    value = filter$get_params("keep_na"),
    label = na_message
  )
}

inherit_parent_stats <- function(filter_values, parent_options, is_cached) {
  if (is_cached || is.null(filter_values)) {
    return(character(0))
  }
  if (identical(filter_values, NA)) {
    return(parent_options)
  } else {
    return(filter_values)
  }
}

discrete_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  step_id <- filter$step_id
  filter_id <- filter$id
  filter_params <- filter$get_params()

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id, choices = character(0), selected = character(0), label = NULL)
    )
  }

  parent_filter_stats <- cohort$get_cache(step_id, filter_id, state = "pre")$choices
  filter_stats <- extend_stats(
    cohort$get_cache(step_id, filter_id, state = "post")$choices,
    parent_filter_stats,
    inherit_parent = inherit_parent_stats(
      filter_params$value,
      names(parent_filter_stats),
      !is.null(cohort$get_cache(step_id, filter_id, state = "post"))
    )
  )
  selected_value <- extract_selected_value(
    filter$get_params("value"),
    parent_filter_stats, reset
  )
  value_mapping <- function(x, cohort) x
  if (!is.null(filter_params$value_mapping)) {
    value_mapping <- cohort$get_source()$attributes$value_mappings[[filter_params$value_mapping]]
  }

  params <- list(
    inputId = input_id,
    choiceValues = names(parent_filter_stats),
    choiceNames = .pre_post_stats_text(
      name = value_mapping(names(parent_filter_stats), cohort),
      current = filter_stats,
      previous = parent_filter_stats,
      stats = if_null_default(
        filter$get_params("stats"),
        cohort$attributes$stats
      )

    ),
    selected = selected_value,
    inline = TRUE,
    label = if (update) character(0) else NULL,
    ...
  )

  if(is_vs(filter)) {
    params$choices <- params$choiceValues %>%
      stats::setNames(params$choiceNames)
    params$choiceValues <- NULL
    params$choiceNames <- NULL
    params$inline <- FALSE
  } else {
    params$choiceNames <- params$choiceNames %>% purrr::map(shiny::HTML)
  }

  return(params)
}

format_number <- function(number) {
  format(number, nsmall = 0, big.mark = " ")
}

plot_feedback_bar <- function(plot_data, n_missing) {

  feedback_data <- data.frame(
    level = factor(names(plot_data)),
    n = unlist(plot_data)
  )

  n_rows <- nrow(feedback_data)
  color_palette <- getOption("scb_chart_palette", scb_chart_palette)$discrete
  n_colors <- length(color_palette)
  chart_cols <- color_palette[rep_len(1:n_colors, n_rows)]


  if (n_missing > 0) {
    feedback_data <- rbind(
      feedback_data,
      data.frame(level = "(missing)", n = n_missing)
    )
    chart_cols <- c(
      chart_cols,
      getOption("scb_chart_palette", scb_chart_palette)$no_data
    )
  }

  if (NROW(feedback_data) == 0) {
    gg_object <- ggplot2::ggplot()
  } else {
    gg_object <-
      feedback_data %>%
      dplyr::mutate(
        tooltip = htmltools::htmlEscape(paste0(level, " (", format_number(n), ")"), TRUE)
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = "I", y = n, fill = level,
          tooltip = paste0(level, " (", format_number(n), ")"),
          data_id = htmltools::htmlEscape(level, TRUE)
        )
      ) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text  = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(0, "pt"),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background  = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = ggplot2::unit(c(0, 0, 0, 0),"mm"),
        panel.border = ggplot2::element_rect(colour = "grey50", fill = NA, size = 1),
        panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "mm")) +
      ggplot2::scale_fill_manual(name = NULL, values = chart_cols) +
      ggiraph::geom_col_interactive(
        position = ggplot2::position_stack(reverse = TRUE)
      )
  }

  ggiraph::girafe(
    ggobj = gg_object,
    width_svg  = 10,
    height_svg = 1.5,
    options = list(
      ggiraph::opts_hover_inv(css = "opacity: 0.2;"),
      ggiraph::opts_tooltip(offx = 10, offy = 10, opacity = 0.5, zindex = 1100),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE),
      ggiraph::opts_toolbar(saveaspng = FALSE)
    )
  )
}

#' @rdname gui-filter-layer
#' @export
.gui_filter.discrete <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_fun <- shiny::checkboxGroupInput
      extra_params <- NULL
      if (is_vs(filter)) {
        input_fun <- shinyWidgets::virtualSelectInput
        extra_params <- list(
          multiple = TRUE,
          html = TRUE,
          search =  TRUE,
          selectAllOnlyVisible = TRUE,
          zIndex = 9999
        )
      }
      shiny::tagList(
        .cb_input(
          do.call(
            input_fun,
            modify_list(
              extra_params,
              discrete_input_params(filter, input_id, cohort, ...)
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
            filter_value <- extract_selected_value(filter$get_params("value"), filter_cache$choices, FALSE)
            plot_data <- filter_cache$choices[filter_value]
            n_missing <- filter_cache$n_missing
            if (identical(filter$get_params("keep_na"), FALSE)) {
              n_missing <- 0
            }

            plot_feedback_bar(plot_data, n_missing)
          })
        }
      )
    },
    server = function(input_id, input, output, session, cohort) {
      shiny::observeEvent(input[[shiny::NS(input_id, "feedback_plot_selected")]], {
        value <- input[[shiny::NS(input_id, "feedback_plot_selected")]]

        if (!is.na(value)) {
          .trigger_action(session, "update_filter", params = list(
            step_id = filter$step_id, filter_id = filter$id,
            input_name = filter$input_param, input_value = value,
            run_flow = FALSE
          ))
        }
      }, ignoreInit = TRUE) %>% .save_observer(input_id, session)
    },
    update = function(session, input_id, cohort, reset = FALSE, ...) {
      input_fun <- shiny::updateCheckboxGroupInput
      update_params <- discrete_input_params(filter, input_id, cohort, reset, TRUE, ...)
      if (is_vs(filter)) {
        input_fun <- shinyWidgets::updateVirtualSelect
        update_params$inline <- NULL
      }
      do.call(
        input_fun,
        append(
          list(session = session),
          update_params
        )
      )
      .update_keep_na_input(session, input_id, filter, cohort)
    },
    post_stats = if (is.null(filter$get_params("stats"))) NULL else "post" %in% filter$get_params("stats"),
    multi_input = FALSE
  )
}
