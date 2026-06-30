#' Generate HTML feedback bar for discrete filter
#' @param plot_data Named list of counts per level
#' @param n_missing Number of missing values
#' @param input_id Input id for click events (NULL to disable clicks)
#' @return shiny::tagList
#' @noRd
html_feedback_bar <- function(plot_data, n_missing, input_id = NULL) {
  if (length(plot_data) == 0 && n_missing == 0) {
    return(shiny::div(class = "cb_fb_bar"))
  }

  levels <- names(plot_data)
  counts <- unlist(plot_data)
  palette <- getOption("scb_chart_palette", scb_chart_palette)$discrete
  n_colors <- length(palette)
  colors <- palette[rep_len(seq_len(n_colors), length(levels))]

  segments <- mapply(function(level, count, color) {
    attrs <- list(
      class = "cb_fb_seg",
      style = sprintf("flex-grow: %s; background: %s;", count, color),
      title = sprintf("%s (%s)", level, format_number(count)),
      `data-value` = level
    )
    if (!is.null(input_id)) {
      session <- shiny::getDefaultReactiveDomain()
      ns <- if (!is.null(session)) session$ns else identity
      click_id <- ns(shiny::NS(input_id, "feedback_bar_clicked"))
      attrs$onclick <- sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        click_id, htmltools::htmlEscape(level, attribute = TRUE)
      )
      attrs$style <- paste0(attrs$style, " cursor: pointer;")
    }
    do.call(shiny::tags$div, attrs)
  }, levels, counts, colors, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  if (n_missing > 0) {
    no_data_color <- getOption("scb_chart_palette", scb_chart_palette)$no_data
    segments <- c(segments, list(
      shiny::tags$div(
        class = "cb_fb_seg",
        style = sprintf("flex-grow: %s; background: %s;", n_missing, no_data_color),
        title = sprintf("(missing) (%s)", format_number(n_missing)),
        `data-value` = "(missing)"
      )
    ))
  }

  shiny::div(class = "cb_fb_bar", segments)
}

#' Generate HTML histogram for range/date filters
#' @param plot_data Data frame with `level` and `count` columns
#' @param n_missing Number of missing values
#' @param n_total Total number of observations
#' @return shiny::tagList
#' @noRd
html_feedback_hist <- function(plot_data, n_missing, n_total, max_bars = 30) {
  color <- getOption("scb_chart_palette", scb_chart_palette)$discrete[1]

  if (NROW(plot_data) > max_bars) {
    bin_size <- ceiling(nrow(plot_data) / max_bars)
    plot_data <- plot_data |>
      dplyr::mutate(.bin = ceiling(dplyr::row_number() / bin_size)) |>
      dplyr::group_by(.bin) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  }

  bars <- NULL
  if (NROW(plot_data) > 0) {
    max_count <- max(plot_data$count, na.rm = TRUE)
    if (max_count > 0) {
      bars <- lapply(seq_len(nrow(plot_data)), function(i) {
        pct <- round(100 * plot_data$count[i] / max_count)
        shiny::tags$div(
          class = "cb_fb_hist_bar",
          style = sprintf("height: %s%%; background: %s;", pct, color)
        )
      })
    }
  }

  subtitle <- shiny::div(
    class = "cb_fb_subtitle",
    sprintf(
      "missing: %s / %s (%s%%)",
      format_number(n_missing),
      format_number(n_total),
      round(n_missing / n_total, 1)
    )
  )

  shiny::tagList(
    shiny::div(class = "cb_fb_hist", bars),
    subtitle
  )
}

#' Generate HTML bar for discrete text filter
#' @param plot_data Named list with `selected` and `not_selected` counts
#' @return shiny::tagList
#' @noRd
html_feedback_text_bar <- function(plot_data) {
  counts <- unlist(plot_data)
  levels <- names(plot_data)
  n_selected <- counts[1]
  n_total <- sum(counts)

  palette <- getOption("scb_chart_palette", scb_chart_palette)
  colors <- c(palette$no_data, palette$discrete[1])

  segments <- mapply(function(level, count, color) {
    shiny::tags$div(
      class = "cb_fb_seg",
      style = sprintf("flex-grow: %s; background: %s;", count, color),
      title = sprintf("%s (%s)", level, format_number(count))
    )
  }, levels, counts, colors, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  subtitle <- shiny::div(
    class = "cb_fb_subtitle",
    sprintf(
      "Unique values: %s / %s (%s%%)",
      format_number(n_selected),
      format_number(n_total),
      round(100 * n_selected / n_total, 1)
    )
  )

  shiny::tagList(
    shiny::div(class = "cb_fb_bar", segments),
    subtitle
  )
}

#' Generate HTML grouped bars for multi-discrete filter
#' @param plot_data Data frame with `variable`, `state`, `value` columns
#' @param n_missing Data frame with missing counts per variable
#' @return shiny::tagList
#' @noRd
html_feedback_multi_bar <- function(plot_data, n_missing) {
  if (NROW(plot_data) == 0) {
    return(shiny::div(class = "cb_fb_bar"))
  }

  states <- unique(plot_data$state)
  palette <- getOption("scb_chart_palette", scb_chart_palette)$discrete
  n_colors <- length(palette)
  state_colors <- stats::setNames(
    palette[rep_len(seq_len(n_colors), length(states))],
    states
  )

  no_data_color <- getOption("scb_chart_palette", scb_chart_palette)$no_data
  variables <- unique(plot_data$variable)

  rows <- lapply(variables, function(var) {
    var_data <- plot_data[plot_data$variable == var, ]
    segments <- lapply(seq_len(nrow(var_data)), function(i) {
      state <- var_data$state[i]
      value <- var_data$value[i]
      color <- state_colors[[state]]
      shiny::tags$div(
        class = "cb_fb_seg",
        style = sprintf("flex-grow: %s; background: %s;", value, color),
        title = sprintf("%s: %s (%s)", var, state, format_number(value))
      )
    })

    # Add missing segment if applicable
    if (!is.null(n_missing)) {
      var_missing <- n_missing[n_missing$variable == var, ]
      if (NROW(var_missing) > 0 && sum(var_missing$value) > 0) {
        segments <- c(segments, list(
          shiny::tags$div(
            class = "cb_fb_seg",
            style = sprintf("flex-grow: %s; background: %s;", sum(var_missing$value), no_data_color),
            title = sprintf("%s: (missing) (%s)", var, format_number(sum(var_missing$value)))
          )
        ))
      }
    }

    shiny::div(class = "cb_fb_bar", segments)
  })

  shiny::tagList(rows)
}
