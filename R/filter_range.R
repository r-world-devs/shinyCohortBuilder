extract_selected_range <- function(range, parent_range, reset) {
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

range_domain_input_params <- function(filter, input_id, cohort, reset = FALSE,
                                      update = FALSE, ...) {
  domain <- cohortBuilder::filter_domain(filter)
  domain_range <- c(domain[1], domain[2])

  if (filter@type == "datetime_range") {
    selected_range <- extract_selected_datetime_range(filter@range, domain_range, reset)
  } else {
    selected_range <- extract_selected_range(filter@range, domain_range, reset)
  }

  params <- list(
    inputId = input_id,
    min = domain_range[1],
    max = domain_range[2],
    value = selected_range,
    label = if (update) character(0) else NULL,
    width = "100%",
    ...
  )

  if (filter@type == "range") {
    params$step <- if (!is.null(filter@extra$step)) filter@extra$step else NULL
  }
  if (filter@type == "date_range") {
    params$start <- params$value[1]
    params$end <- params$value[2]
    params$value <- NULL
  }
  if (filter@type == "datetime_range") {
    params$step <- if (!is.null(filter@extra$step)) filter@extra$step else NULL
    params$min <- as.POSIXct(params$min, origin = "1970-01-01 UTC")
    params$max <- as.POSIXct(params$max, origin = "1970-01-01 UTC")
    params$value <- c(
      as.POSIXct(params$value[1], origin = "1970-01-01 UTC"),
      as.POSIXct(params$value[2], origin = "1970-01-01 UTC")
    )
  }
  if (update) {
    params$width <- NULL
  }

  params
}

range_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  step_id <- filter@step_id
  filter_id <- filter@id

  render <- resolve_render_mode(filter, cohort)
  domain <- cohortBuilder::filter_domain(filter)

  # Domain mode (or stats mode with render_source = "domain"): build bounds from
  # the declared domain without reading the cache.
  use_domain <- render$mode == "domain" ||
    (identical(render$render_source, "domain") && !is.null(domain))

  if (render$mode == "domain" && is.null(domain)) {
    warn_no_domain(filter_id)
    return(range_input_defaults(input_id, filter@type))
  }
  if (identical(render$render_source, "domain") && is.null(domain) &&
      render$mode == "stats") {
    inform_domain_fallback(filter_id)
  }

  if (use_domain) {
    return(
      range_domain_input_params(filter, input_id, cohort, reset = reset, update = update, ...)
    )
  }

  if (!cohort$get_cache(step_id, filter_id, state = "pre", name = "n_data")) {
    return(
      range_input_defaults(input_id, filter@type)
    )
  }

  parent_filter_stats <- cohort$get_cache(step_id, filter_id, state = "pre", name = "frequencies")
  parent_range <- freq_range(parent_filter_stats)

  if (filter@type == "datetime_range") {
    selected_range <- extract_selected_datetime_range(
      filter@range,
      parent_range, reset
    )
  } else {
    selected_range <- extract_selected_range(
      filter@range,
      parent_range, reset
    )
  }


  params <- list(
    inputId = input_id,
    min = parent_range[1],
    max = parent_range[2],
    value = selected_range,
    label = if (update) character(0) else NULL,
    width = "100%",
    ...
  )

  # Below should be deprecated now soon
  if (filter@type == "range") {
    if (!is.null(filter@extra$step)) {
      params$step <- filter@extra$step
    } else {
      params$step <- freq_step(parent_filter_stats)
    }
  }
  if (filter@type == "date_range") {
    params$start <- params$value[1]
    params$end <- params$value[2]
    params$value <- NULL
  }

  if (filter@type == "datetime_range") {
    if (!is.null(filter@extra$step)) {
      params$step <- filter@extra$step
    } else {
      params$step <- freq_step(parent_filter_stats)
    }
    params$min <- as.POSIXct(params$min, origin = "1970-01-01 UTC")
    params$max <- as.POSIXct(params$max, origin = "1970-01-01 UTC")
    params$value <- c(
      as.POSIXct(params$value[1], origin = "1970-01-01 UTC"),
      as.POSIXct(params$value[2], origin = "1970-01-01 UTC")
    )
  }

  if (update) {
    params$width <- NULL
  }

  return(params)
}

suff_id <- function(params_list, suffix) {
  params_list$inputId <- paste0(params_list$inputId, "-", suffix)
  return(params_list)
}

is_gui_type <- function(filter, type) {
  gui_input <- filter@extra$gui_input
  if (is.null(gui_input)) {
    return(TRUE)
  }
  type %in% filter@extra$gui_input
}

S7::method(.gui_filter, cohortBuilder::CbFilterRange) <- function(object, ...) {
  list(
    input = function(filter, input_id, cohort) {
      input_params <- range_input_params(filter, input_id, cohort, ...)

      shiny::tagList(
        if (is_gui_type(filter, "slider")) {
          .cb_input(
            do.call(
              shiny::sliderInput,
              modify_list(
                list(ticks = FALSE, round = -2),
                suff_id(input_params, "slider")
              )
            ),
            filter@private$input_param
          )
        },
        if (is_gui_type(filter, "numeric")) {
          .cb_input(
            do.call(
              shinyWidgets::numericRangeInput,
              suff_id(input_params, "numrange")
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
            if (empty) {
              return(shiny::div(class = "cb_fb_bar"))
            }
            step_id <- filter@step_id
            filter_id <- filter@id

            filter_cache <- cohort$get_cache(step_id, filter_id, state = "pre")
            filter_range <- extract_selected_range(
              filter@range,
              freq_range(filter_cache$frequencies),
              FALSE
            )

            plot_data <- filter_cache$frequencies |>
              dplyr::mutate(
                count = ifelse(l_bound >= filter_range[1] & l_bound <= filter_range[2], count, 0)
              )
            n_missing <- filter_cache$n_missing
            n_total <- filter_cache$n_data
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
    multi_input = length(object@extra$gui_input) != 1
  )
}
