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
  name <- empty_if_false(!missing(name), paste0(name, " "), FALSE, "")
  open_bracket <- empty_if_false(brackets && any(stats %in% c("pre", "post")), "(", FALSE, "")
  post_stat <- empty_if_false(
    "post" %in% stats,
    glue::glue("<span class = 'cb_delayed'>{if_na_default(current, '??')}</span>"),
    FALSE, ""
  )
  slash <- empty_if_false(length(stats) == 2, " / ", FALSE, "")
  pre_stat <- empty_if_false("pre" %in% stats, previous, FALSE, "")
  close_bracket <- empty_if_false(brackets && any(stats %in% c("pre", "post")), ")", FALSE, "")
  percent_open_bracket <- empty_if_false(percent && length(stats) == 2, " (", FALSE, "")
  percentage <- empty_if_false(
    percent && length(stats) == 2,
    glue::glue("<span class = 'cb_delayed'>{calc_percent(current, previous)}%</span>"),
    FALSE, ""
  )
  percent_close_bracket <- empty_if_false(percent && length(stats) == 2, ")", FALSE, "")
  glue::glue(
    "<span>",
    "{name}{open_bracket}{post_stat}{slash}{pre_stat}{close_bracket}",
    "{percent_open_bracket}{percentage}{percent_close_bracket}",
    "</span>"
  )
}

is_vs <- function(filter) {
  !is.null(filter@extra$gui_input) && filter@extra$gui_input == "vs"
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
#' library(cohortBuilder)
#'
#' librarian_source <- set_source(as.tblist(librarian))
#' coh <- cohort(
#'   librarian_source,
#'   filter(
#'     "range", id = "copies", name = "Copies", dataset = "books",
#'     variable = "copies", range = c(5, 12)
#'   )
#' ) |> run()
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

  filter_id <- filter@id
  step_id <- filter@step_id
  na_message <- keep_na_message(filter, cohort, msg_fun)

  shiny::tagList(
    shiny::checkboxInput(
      paste0(input_id, "-keep_na"),
      label = na_message,
      filter@keep_na
    ) |>
      shiny::tagAppendAttributes(class = "cb_na_input")
  )
}

# Build the keep-NA checkbox label. In stats mode it includes the missing-value
# count from the cache; in domain mode (stats disabled) it uses a neutral label
# without reading the cache.
keep_na_message <- function(filter, cohort, msg_fun) {
  render <- resolve_render_mode(filter, cohort)
  if (render$mode == "domain") {
    return("Keep missing values")
  }
  cohort$get_cache(filter@step_id, filter@id, state = "pre")$n_missing |>
    msg_fun()
}

#' @rdname keep_na_input
#' @export
.update_keep_na_input <- function(session, input_id, filter, cohort,
                                  msg_fun = function(x) glue::glue("Keep missing values ({x})")) {

  filter_id <- filter@id
  step_id <- filter@step_id
  na_message <- keep_na_message(filter, cohort, msg_fun)
  shiny::updateCheckboxInput(
    session,
    inputId = paste0(input_id, "-keep_na"),
    value = filter@keep_na,
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

discrete_domain_input_params <- function(filter, input_id, cohort, reset = FALSE,
                                         update = FALSE, counts = NULL, ...) {
  filter_params <- get_filter_params(filter)
  domain <- cohortBuilder::filter_domain(filter)

  value_mapping <- function(x, cohort) x
  if (!is.null(filter_params$value_mapping)) {
    value_mapping <- cohort$get_source()$attributes$value_mappings[[filter_params$value_mapping]]
  }

  selected_value <- if (reset) {
    domain
  } else {
    suppressWarnings(cohortBuilder::filter_effective_value(filter))
  }
  if (identical(selected_value, NA)) {
    selected_value <- domain
  }

  choice_labels <- value_mapping(domain, cohort)
  # In stats mode with render_source = "domain", overlay counts where available.
  if (!is.null(counts)) {
    overlay <- counts[domain]
    overlay[is.na(overlay)] <- 0
    choice_labels <- glue::glue(
      "<span>{choice_labels} (<span class = 'cb_delayed'>{overlay}</span>)</span>"
    )
  }

  params <- list(
    inputId = input_id,
    choiceValues = domain,
    choiceNames = choice_labels,
    selected = selected_value,
    inline = TRUE,
    label = if (update) character(0) else NULL,
    ...
  )

  if (is_vs(filter)) {
    params$choices <- params$choiceValues |>
      stats::setNames(params$choiceNames)
    params$choiceValues <- NULL
    params$choiceNames <- NULL
    params$inline <- FALSE
  } else {
    params$choiceNames <- params$choiceNames |> purrr::map(shiny::HTML)
  }

  params
}

discrete_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id
  filter_params <- get_filter_params(filter)

  render <- resolve_render_mode(filter, cohort)
  domain <- cohortBuilder::filter_domain(filter)

  # Domain mode: render from the declared domain without touching the cache.
  if (render$mode == "domain") {
    if (is.null(domain)) {
      warn_no_domain(filter_id)
      return(
        list(inputId = input_id, choices = character(0), selected = character(0), label = NULL)
      )
    }
    return(
      discrete_domain_input_params(filter, input_id, cohort, reset = reset, update = update, ...)
    )
  }

  # Stats mode, but render_source = "domain": build choices from the full domain
  # and overlay counts from statistics where available.
  if (identical(render$render_source, "domain")) {
    if (is.null(domain)) {
      inform_domain_fallback(filter_id)
    } else {
      counts <- cohort$get_cache(step_id, filter_id, state = "post")$choices
      return(
        discrete_domain_input_params(
          filter, input_id, cohort, reset = reset, update = update, counts = counts, ...
        )
      )
    }
  }

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
    filter@value,
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
        filter_params$stats,
        cohort$attributes$stats
      )

    ),
    selected = selected_value,
    inline = TRUE,
    label = if (update) character(0) else NULL,
    ...
  )

  if(is_vs(filter)) {
    params$choices <- params$choiceValues |>
      stats::setNames(params$choiceNames)
    params$choiceValues <- NULL
    params$choiceNames <- NULL
    params$inline <- FALSE
  } else {
    params$choiceNames <- params$choiceNames |> purrr::map(shiny::HTML)
  }

  return(params)
}

format_number <- function(number) {
  format(number, nsmall = 0, big.mark = " ")
}

S7::method(.gui_filter, cohortBuilder::CbFilterDiscrete) <- function(object, ...) {
  list(
    input = function(filter, input_id, cohort) {
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
          filter@private$input_param
        ),
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
            filter_value <- extract_selected_value(filter@value, filter_cache$choices, FALSE)
            plot_data <- filter_cache$choices[filter_value]
            n_missing <- filter_cache$n_missing
            if (identical(filter@keep_na, FALSE)) {
              n_missing <- 0
            }

            html_feedback_bar(plot_data, n_missing, input_id = input_id)
          })
        }
      )
    },
    server = function(filter, input_id, input, output, session, cohort) {
      shiny::observeEvent(input[[shiny::NS(input_id, "feedback_bar_clicked")]], {
        value <- input[[shiny::NS(input_id, "feedback_bar_clicked")]]

        if (!is.na(value)) {
          .trigger_action(session, "update_filter", params = list(
            step_id = filter@step_id, filter_id = filter@id,
            input_name = filter@private$input_param, input_value = value,
            update = "force_input", run_flow = FALSE
          ))
        }
      }, ignoreInit = TRUE) |> .save_observer(input_id, session)
    },
    update = function(filter, session, input_id, cohort, reset = FALSE, ...) {
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
    post_stats = if (is.null(object@extra$stats)) NULL else "post" %in% object@extra$stats,
    multi_input = FALSE
  )
}
