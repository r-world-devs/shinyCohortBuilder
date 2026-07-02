#' Resolve a multi_discrete filter's per-group selected values
#'
#' Returns all choices per group on reset, drops `NA` group selections, and
#' overlays the remaining selections onto the full set of choices.
#'
#' @param values Named list of per-group selected values.
#' @param parent_filter_stats Named list of per-group available choices.
#' @param reset When `TRUE`, select all choices in every group.
#' @return A named list of per-group selected values.
#' @noRd
extract_selected_values <- function(values, parent_filter_stats, reset) {

  all_choices <- purrr::map(parent_filter_stats, names)
  if (reset) {
    return(all_choices)
  }
  filtered_selection <- values |> purrr::keep(~!identical(., NA))
  if (!length(filtered_selection)) {
    filtered_selection <- list()
  }
  utils::modifyList(
    all_choices,
    filtered_selection,
    keep.null = TRUE
  )[names(values)]
}

#' Build pre/post choice labels for a multi_discrete group
#' @param name Vector of choice label texts.
#' @param parent_stat Parent (pre) counts.
#' @param current_stat Current (post) counts.
#' @param stats Which stats to show (`"pre"`/`"post"`).
#' @return A list of HTML choice labels.
#' @noRd
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

#' Fill in groups missing from a multi_discrete stats list
#'
#' Adds empty entries for any parent group absent from `init`, then reorders to
#' match the parent.
#'
#' @param init Named list of per-group stats (possibly incomplete).
#' @param parent Named list of parent per-group stats.
#' @return `init` extended and reordered to the parent's groups.
#' @noRd
complete_stats_list <- function(init, parent) {
  missing_stats <- setdiff(names(parent), names(init))
  for (missing_stat in missing_stats) {
    init[[missing_stat]] <- list()
  }
  init[names(parent)]
}

#' Pair-wise apply names to a list of vectors
#' @param list_vals List of vectors to name.
#' @param list_names List of name vectors, aligned to `list_vals`.
#' @return The list of vectors with names applied.
#' @noRd
attach_list_names <- function(list_vals, list_names) {
  purrr::map2(
    list_vals,
    list_names,
    ~ stats::setNames(.x, .y)
  )
}

#' Build pickCheckbox input params for a multi_discrete filter
#'
#' Assembles per-group choices, pre/post labelled choice names and the resolved
#' selection from cached stats; returns empty params when the parent step holds
#' no data.
#'
#' @param filter A cohortBuilder filter object.
#' @param input_id Base input id.
#' @param cohort The cohort object.
#' @param reset When `TRUE`, select all choices in every group.
#' @param update When `TRUE`, build params for an update (vs initial render).
#' @param ... Extra params forwarded to the input constructor.
#' @return A named list of input constructor params.
#' @noRd
multi_discrete_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id
  filter_params <- get_filter_params(filter)

  max_groups <- length(cohort$get_stats("1", filter_id, state = "pre", name = "choices"))

  if (!cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data")) {
    return(
      list(inputId = input_id, label = NULL, choices = NULL, choicesNames = NULL, selected = NULL, max_groups = max_groups)
    )
  }

  parent_filter_stats <- cohort$get_stats(step_id, filter_id, state = "pre", name = "choices")
  filter_stats <- complete_stats_list(
    cohort$get_stats(step_id, filter_id, state = "post", name = "choices"),
    parent_filter_stats
  ) |>
    purrr::map2(parent_filter_stats, extend_stats)

  selected_value <- extract_selected_values(
    filter@values,
    parent_filter_stats, reset
  )
  choices <- parent_filter_stats |> purrr::map(names)
  choices_names <- shinyGizmo::pickCheckboxNames(choices)

  value_mapping <- function(x, cohort) x
  if (!is.null(filter_params$value_mapping)) {
    value_mapping <- cohort$get_source()$attributes$value_mappings[[filter_params$value_mapping]]
  }

  choices_labels <- value_mapping(
    shinyGizmo::pickCheckboxLabels(choices),
    cohort
  )

  choices_names <- purrr::pmap(
    list(
      name = choices_names,
      current_stat = filter_stats,
      parent_stat = parent_filter_stats
    ),
    choice_names,
    stats = if_null_default(
      filter@extra$stats,
      cohort$attributes$stats
    )
  )

  params <- list(
    inputId = input_id,
    choices = choices,
    choicesNames = choices_names,
    choicesLabels = choices_labels,
    selected = selected_value,
    label = NULL,
    ...
  )

  if (update) {
    params$label <- NULL
  }

  return(params)
}

#' Flatten a grouped per-state count list into a long data frame
#' @param grouped_list Named list of per-group named count vectors.
#' @return A list of per-group data frames with `variable`, `state`, `value`.
#' @noRd
grouped_list_to_df <- function(grouped_list) {
  grouped_list |>
    purrr::keep(~length(.) > 0) |>
    purrr::imap(
      function(x, y) data.frame(variable = y, data.frame(state = names(x), value = unlist(x)))
    )
}

S7::method(.gui_filter, cohortBuilder::CbFilterMultiDiscrete) <- function(object, ...) {
  list(
    input = function(filter, input_id, cohort) {
      shiny::tagList(
        .cb_input(
          do.call(
            shinyGizmo::pickCheckboxInput,
            modify_list(
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
          filter@private$input_param
        ),
        .cb_input(
          .keep_na_input(
            input_id, filter, cohort,
            msg_fun = function(x) "Keep missing values"
          ),
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
            orig_values <- filter@values
            if (is.null(orig_values)) {
              orig_values <- filter_stats$choices |>
                purrr::map(names)
            } else {
              orig_values <- orig_values |>
                purrr::map(~as.character(unlist(.)))
            }
            filter_value <- purrr::map2(
              stats::setNames(orig_values[names(filter_stats$choices)], names(filter_stats$choices)),
              filter_stats$choices,
              ~extract_selected_value(.x, .y, FALSE)
            )
            plot_data <- filter_stats$choices |>
              purrr::imap(function(x, y) {x[unlist(filter_value[y])]}) |>
              grouped_list_to_df() |>
              dplyr::bind_rows()
            n_missing <- data.frame(
              variable = names(filter_stats$n_missing),
              state = "(missing)",
              value = unlist(filter_stats$n_missing)
            ) |>
              dplyr::filter(variable %in% plot_data$variable)
            if (identical(filter@keep_na, FALSE)) {
              n_missing$value <- 0
            }

            html_feedback_multi_bar(plot_data, n_missing)
          })
        }
      )
    },
    server = function(filter, input_id, input, output, session, cohort) {},
    update = function(filter, session, input_id, cohort, reset = FALSE, ...) {
      update_params <- multi_discrete_input_params(filter, input_id, cohort, reset, TRUE, ...)
      update_params$max_groups <- NULL
      update_params$label <- NULL
      do.call(
        shinyGizmo::updatePickCheckboxInput,
        append(
          list(session = session),
          update_params
        )
      )
      .update_keep_na_input(
        session, input_id, filter, cohort,
        msg_fun = function(x) "Keep missing values"
      )
    },
    post_stats = TRUE,
    multi_input = FALSE
  )
}
