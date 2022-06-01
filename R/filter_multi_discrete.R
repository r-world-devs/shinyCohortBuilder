extract_selected_values <- function(values, parent_filter_stats, reset) {

  all_choices <- purrr::map(parent_filter_stats, names)
  if (reset) {
    return(all_choices)
  }
  utils::modifyList(
    all_choices,
    values %>% purrr::keep(~!identical(., NA)),
    keep.null = TRUE
  )[names(values)]
}

choice_names <- function(name, parent_stat, current_stat, stats) {
  purrr::pmap(
    list(
      current = current_stat,
      previous = parent_stat,
      name = name
    ),
    .pre_post_stats,
    brackets = TRUE, stats = stats
  )
}

complete_stats_list <- function(init, parent) {
  missing_stats <- setdiff(names(parent), names(init))
  for (missing_stat in missing_stats) {
    init[[missing_stat]] <- list()
  }
  init[names(parent)]
}

attach_list_names <- function(list_vals, list_names) {
  purrr::map2(
    list_vals,
    list_names,
    ~ stats::setNames(.x, .y)
  )
}

multi_discrete_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  step_id <- filter$step_id
  filter_id <- filter$id
  max_groups <- length(cohort$get_cache("1", filter_id, state = "pre")$choices)

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id, choices = NULL, choices_names = NULL, selected = NULL, max_groups = max_groups)
    )
  }

  parent_filter_stats <- cohort$get_cache(step_id, filter_id, state = "pre")$choices
  filter_stats <- complete_stats_list(
    cohort$get_cache(step_id, filter_id, state = "post")$choices,
    parent_filter_stats
  ) %>%
    purrr::map2(parent_filter_stats, extend_stats)

  selected_value <- extract_selected_values(
    filter$get_params("values"),
    parent_filter_stats, reset
  )
  choices <- parent_filter_stats %>% purrr::map(names)
  choices_names <- shinyGizmo::pickCheckboxNames(choices)
  choices_labels <- shinyGizmo::pickCheckboxLabels(choices)

  choices_names <- purrr::pmap(
    list(
      name = choices_names,
      current_stat = filter_stats,
      parent_stat = parent_filter_stats
    ),
    choice_names,
    stats = cohort$attributes$stats
  )

  # if (!is.null(filter$get_params("choice_names"))) {
  #   filter$get_params("choice_names")
  # }  else {
  #   nested_choice_names
  # }
  params <- list(
    inputId = input_id,
    choices = choices,
    choicesNames = choices_names,
    choicesLabels = choices_labels,
    selected = selected_value,
    ...
  )

  return(params)
}

plot_feedback_multi_bar <- function(plot_data, n_missing) {

  if (NROW(plot_data) == 0) {
    gg_object <- ggplot2::ggplot()
  } else {
    add_greycol <- FALSE
    if (sum(n_missing$value) > 0) {
      plot_data <- dplyr::bind_rows(
        plot_data,
        n_missing
      )
      add_greycol <- TRUE
    }

    color_palette <- scb_color_palette
    colors_selected <-
      color_palette$shades["grayscale" != names(color_palette$shades)]
    colors_selected <- colors_selected %>% unlist
    colors_selected <- colors_selected[seq(3, 6 * 4, by = 4)]
    colors_selected <- colors_selected[c(1, 2, 6, 4, 5)]
    colors_selected <- rev(colors_selected)
    colors_selected <- rep(colors_selected, 1000)
    colors_selected <- unname(colors_selected)
    colors_selected <- colors_selected[1:length(unique(plot_data$state))]

    if (add_greycol) {
      colors_selected[length(colors_selected)] <- "grey40"
    }

    gg_object <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = variable,
          y = value,
          fill = state,
          tooltip = paste0(variable, ": ", state, " (", format_number(value), ")"),
          data_id = state
        )
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(expand = c(0, 0), limits = rev(unique(plot_data$variable))) +
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
        panel.border = ggplot2::element_rect(
          colour = "grey50",
          fill = NA,
          size = 1
        ),
        panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "mm")
      ) +
      ggplot2::scale_fill_manual(name = NULL, breaks = unique(plot_data$state), values = colors_selected) +
      ggiraph::geom_bar_interactive(
        position = ggplot2::position_stack(reverse = TRUE), stat = "identity", width = 1
      )
  }

  ggiraph::girafe(
    ggobj      = gg_object,
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

grouped_list_to_df <- function(grouped_list) {
  grouped_list %>%
    purrr::keep(~length(.) > 0) %>%
    purrr::imap(
      function(x, y) data.frame(variable = y, data.frame(state = names(x), value = unlist(x)))
    )
}

#' @rdname gui-filter-layer
#' @export
.gui_filter.multi_discrete <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      .cb_input(
        do.call(
          shinyGizmo::pickCheckboxInput,
          append(
            list(
              options  = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                size = 10,
                dropdownAlignRight = 'auto',
                liveSearch = TRUE,
                liveSearchNormalize = TRUE
              )
            ),
            multi_discrete_input_params(filter, input_id, cohort, ...)
          )
        ),
        filter$input_param
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
            filter_value <- purrr::map2(
              stats::setNames(filter$get_params("values")[names(filter_cache$choices)], names(filter_cache$choices)),
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
            )
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
      update_params <- multi_discrete_input_params(filter, input_id, cohort, reset, TRUE, ...)
      update_params$max_groups <- NULL
      do.call(
        shinyGizmo::updatePickCheckboxInput,
        append(
          list(session = session),
          update_params
        )
      )
    },
    post_stats = TRUE,
    multi_input = FALSE
  )
}
