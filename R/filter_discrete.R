#' Fill a child stats list with the parent's missing categories
#'
#' Adds any categories present in `parent` but absent from `current` as `0`
#' counts (or the parent's count for names listed in `inherit_parent`), then
#' reorders to match the parent.
#'
#' @param current Named list/vector of current (post) counts.
#' @param parent Named list/vector of parent (pre) counts.
#' @param inherit_parent Names whose value should be copied from the parent
#'   rather than zeroed.
#' @return `current` extended and reordered to the parent's names.
#' @noRd
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

#' Resolve a discrete filter's selected value against available choices
#'
#' Returns all parent choices on reset or `NA`, passes `NULL` through, and
#' otherwise intersects the selection with the available choices.
#'
#' @param value The selected value(s) (or `NA`/`NULL`).
#' @param parent_filter_stats Named stats whose names are the available choices.
#' @param reset When `TRUE`, select all choices.
#' @return The resolved character vector of selected choices.
#' @noRd
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

#' Build a discrete choice label with pre/post counts
#' @param name Choice label text.
#' @param parent_stat Parent (pre) count.
#' @param current_stat Current (post) count.
#' @param stats Which stats to show (`"pre"`/`"post"`).
#' @return An HTML choice label.
#' @noRd
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

#' Does the filter use the virtualSelect ("vs") GUI input?
#' @param filter A cohortBuilder filter object.
#' @return `TRUE` when `filter@extra$gui_input == "vs"`.
#' @noRd
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

#' Build the keep-NA checkbox label
#'
#' In stats mode it includes the missing-value count from the stats; in domain
#' mode (stats disabled) it uses a neutral label without reading the stats.
#'
#' @param filter A cohortBuilder filter object.
#' @param cohort The cohort object.
#' @param msg_fun Function mapping a missing-value count to a label.
#' @return The checkbox label string.
#' @noRd
keep_na_message <- function(filter, cohort, msg_fun) {
  render <- resolve_render_mode(filter, cohort)
  if (render$mode == "domain") {
    return("Keep missing values")
  }
  cohort$get_stats(filter@step_id, filter@id, state = "pre", name = "n_missing") |>
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

#' Decide which categories should inherit the parent's counts
#'
#' Selected categories with no cached post stats should display the parent's
#' counts rather than zero; returns the names eligible for that inheritance.
#'
#' @param filter_values The filter's selected value(s) (or `NA`/`NULL`).
#' @param parent_options Names of the parent's available choices.
#' @param is_cached Whether post stats are already cached.
#' @return Character vector of category names to inherit, possibly empty.
#' @noRd
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

#' Align a discrete counts cache to the filter's full domain
#'
#' Returns a named integer vector over `domain` with absent values filled as 0.
#' Indexing the list with `[domain]` is unsafe: missing names yield `NULL`
#' elements (named `<NA>`) that `is.na()` does not flag, which leaks a literal
#' "NULL" into the rendered label.
#'
#' @param counts Named list/vector of counts (the `choices` cache).
#' @param domain Character vector of all domain values.
#' @return A named integer vector over `domain`.
#' @noRd
.align_domain_counts <- function(counts, domain) {
  vapply(
    domain,
    function(value) {
      count <- counts[[value]]
      if (is.null(count)) 0L else as.integer(count)
    },
    integer(1)
  )
}

#' Build discrete input params from the filter's declared domain
#'
#' Renders choices from the full domain (no stats scan). When `pre`/`post`
#' counts are supplied (stats mode with `render_source = "domain"`), overlays
#' pre/post labels aligned to the domain.
#'
#' @param filter A cohortBuilder filter object.
#' @param input_id Base input id.
#' @param cohort The cohort object.
#' @param reset When `TRUE`, select the full domain.
#' @param update When `TRUE`, build params for an update (vs initial render).
#' @param pre,post Optional pre/post `choices` counts to overlay on labels.
#' @param stats Which stats to show in labels.
#' @param ... Extra params forwarded to the input constructor.
#' @return A named list of input constructor params.
#' @noRd
discrete_domain_input_params <- function(filter, input_id, cohort, reset = FALSE,
                                         update = FALSE, pre = NULL, post = NULL,
                                         stats = NULL, ...) {
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
  # In stats mode with render_source = "domain", overlay pre/post counts aligned
  # to the full domain (absent values shown as 0), matching the pre/post display
  # used in regular stats mode.
  if (!is.null(stats) && (!is.null(pre) || !is.null(post))) {
    choice_labels <- .pre_post_stats_text(
      name = value_mapping(domain, cohort),
      current = .align_domain_counts(post, domain),
      previous = .align_domain_counts(pre, domain),
      stats = stats
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

#' Resolve discrete input params for the active render mode
#'
#' Dispatches between domain mode, stats mode with `render_source = "domain"`,
#' and plain stats mode (building pre/post labelled choices from cached stats),
#' returning empty choices when there is nothing to render.
#'
#' @param filter A cohortBuilder filter object.
#' @param input_id Base input id.
#' @param cohort The cohort object.
#' @param reset When `TRUE`, select all available choices.
#' @param update When `TRUE`, build params for an update (vs initial render).
#' @param ... Extra params forwarded to the input constructor.
#' @return A named list of input constructor params.
#' @noRd
discrete_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id
  filter_params <- get_filter_params(filter)

  render <- resolve_render_mode(filter, cohort)
  domain <- cohortBuilder::filter_domain(filter)

  # Domain mode: render from the declared domain without touching the stats.
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
  # and overlay pre/post counts from statistics, aligned to the domain.
  if (identical(render$render_source, "domain")) {
    if (is.null(domain)) {
      inform_domain_fallback(filter_id)
    } else {
      return(
        discrete_domain_input_params(
          filter, input_id, cohort, reset = reset, update = update,
          pre = cohort$get_stats(step_id, filter_id, state = "pre", name = "choices"),
          post = cohort$get_stats(step_id, filter_id, state = "post", name = "choices"),
          stats = if_null_default(filter_params$stats, cohort$attributes$stats),
          ...
        )
      )
    }
  }

  if (!cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data")) {
    return(
      list(inputId = input_id, choices = character(0), selected = character(0), label = NULL)
    )
  }

  parent_filter_stats <- cohort$get_stats(step_id, filter_id, state = "pre", name = "choices")
  filter_stats <- extend_stats(
    cohort$get_stats(step_id, filter_id, state = "post", name = "choices"),
    parent_filter_stats,
    inherit_parent = inherit_parent_stats(
      filter_params$value,
      names(parent_filter_stats),
      !is.null(cohort$get_stats(step_id, filter_id, state = "post"))
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

#' Format an integer with a thin-space thousands separator
#' @param number Numeric value to format.
#' @return A formatted number string.
#' @noRd
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

            filter_stats <- cohort$get_stats(step_id, filter_id, state = "pre")
            filter_value <- extract_selected_value(filter@value, filter_stats$choices, FALSE)
            plot_data <- filter_stats$choices[filter_value]
            n_missing <- filter_stats$n_missing
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
