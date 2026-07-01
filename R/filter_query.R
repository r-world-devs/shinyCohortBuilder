#' Get the first list element whose `$id` matches
#' @param list_obj A list of objects each carrying an `id`.
#' @param id Id to match.
#' @return The matching element.
#' @noRd
take_from_list <- function(list_obj, id) {
  purrr::keep(list_obj, ~ .$id == id)[[1]]
}

#' Clamp/intersect a query rule value against its filter config
#'
#' Intersects with the allowed `values`, or clamps numeric values to the
#' config's `min`/`max` validation bounds.
#'
#' @param val The rule's value(s).
#' @param filter_config The queryBuilder filter config for this variable.
#' @return The adjusted value(s).
#' @noRd
rematch_with_validation <- function(val, filter_config) {
  config_vars <- names(filter_config)
  if ("values" %in% config_vars) {
    return(intersect(val, filter_config$values))
  }
  if ("validation" %in% config_vars) {
    validation_stats <- filter_config[["validation"]]
    if ("min" %in% names(validation_stats)) {
      val[val < validation_stats$min] <- validation_stats$min
    }
    if ("max" %in% names(validation_stats)) {
      val[val > validation_stats$max] <- validation_stats$max
    }
    return(val)
  }
  return(val)
}

#' Derive a default query rule value from its filter config
#'
#' Prefers `default_value`, then `values`, then the `min`/`max` validation
#' bounds; errors when none are present.
#'
#' @param filter_config The queryBuilder filter config for a variable.
#' @return The derived default value(s).
#' @noRd
get_val_from_validation <- function(filter_config) {
  config_vars <- names(filter_config)
  if ("default_value" %in% config_vars) {
    return(filter_config$default_value)
  }
  if ("values" %in% config_vars) {
    return(filter_config$values)
  }
  if ("validation" %in% config_vars) {
    validation_stats <- filter_config[["validation"]]
    val <- c()
    if ("min" %in% names(validation_stats)) {
      val[1] <- validation_stats$min
    }
    if ("max" %in% names(validation_stats)) {
      val[length(val) + 1] <- validation_stats$max
    }
    return(val)
  }
  stop("Couldn't extract new filter value from validation setting.")
}

#' Adapt a single query rule's value to its filter limits
#'
#' Uses the config default on reset/`NA`, otherwise clamps/intersects the
#' existing value. Errors when the variable's validation is unset.
#'
#' @param rule A single query rule (`$id`, `$value`).
#' @param filters List of queryBuilder filter configs.
#' @param reset When `TRUE`, use the config default value.
#' @return The adapted rule value.
#' @noRd
adapt_vals_to_limits <- function(rule, filters, reset) {
  filter_config <- take_from_list(filters, rule$id)
  validation_vars <- c("values", "validation")
  validation_var <- validation_vars[validation_vars %in% names(filter_config)]
  if (length(validation_var) != 1) {
    stop("It's required to have the filter validation set.")
  }

  if (identical(rule$value, NA) || reset) {
    rule$value <- get_val_from_validation(filter_config)
  } else {
    rule$value <- rematch_with_validation(rule$value, filter_config)
  }

  return(rule$value)
}

#' Recursively adapt query rule values to their filter limits
#'
#' Walks a (possibly nested) query rule tree, adapting each leaf rule's value to
#' its filter config.
#'
#' @param rules A query rule or rule group.
#' @param filters List of queryBuilder filter configs.
#' @param reset When `TRUE`, use config defaults.
#' @return The rules with adapted values.
#' @noRd
adapt_rules_vals_to_limits <- function(rules, filters, reset) {

  if (!is.null(rules$condition)) {
    rules$rules <- purrr::modify(rules$rules, adapt_rules_vals_to_limits, filters = filters, reset = reset)
  } else {
    rules$value <- adapt_vals_to_limits(rules, filters, reset)
  }

  return(rules)
}

#' Adapt a query rule tree to filter limits, tolerating empty input
#'
#' Returns an empty list for `NA`/`NULL` rules, otherwise delegates to
#' [adapt_rules_vals_to_limits()].
#'
#' @param rules A query rule tree (or `NA`/`NULL`).
#' @param filters List of queryBuilder filter configs.
#' @param reset When `TRUE`, use config defaults.
#' @return The adapted rules, or an empty list.
#' @noRd
adapt_rules_to_limits <- function(rules, filters, reset = FALSE) {

  if (identical(rules, NA) || identical(rules, NULL)) {
    return(list())
  }
  rules <- adapt_rules_vals_to_limits(rules, filters, reset)

  return(rules)
}

#' Build queryBuilder input params for a query filter
#'
#' Constructs queryBuilder filter specs from the parent step's cached specs and
#' adapts the selected rules to those limits; returns minimal params when the
#' parent step holds no data.
#'
#' @param filter A cohortBuilder filter object.
#' @param input_id Base input id.
#' @param cohort The cohort object.
#' @param reset When `TRUE`, reset rule values to defaults.
#' @param update When `TRUE`, build params for an update (vs initial render).
#' @param ... Extra GUI args (e.g. per-variable `filters` overrides).
#' @return A named list of input constructor params.
#' @noRd
query_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {
  input_id <- suff(input_id, "val")
  step_id <- filter@step_id
  filter_id <- filter@id

  if (!cohort$get_stats(step_id, filter_id, state = "pre", name = "n_data")) {
    return(
      list(inputId = input_id)
    )
  }

  gui_args <- list(...)
  if (is.null(gui_args$filters)) {
    gui_args$filters <- list()
  }

  parent_specs <- cohort$get_stats(step_id, filter_id, state = "pre", name = "specs")
  setting_from_stat <- base::get("setting_from_stat", envir = asNamespace("shinyQueryBuilder"), inherits = FALSE)

  filters <- filter@variables |>
    purrr::map(
      ~ setting_from_stat(
        parent_specs[[.x]], .x, !!!gui_args$filters[[.x]],
        .queryBuilderConfig = queryBuilder::queryBuilderConfig
      )
    ) |>
    purrr::map(~rlang::inject(shinyQueryBuilder::queryFilter(!!!.x)))
  gui_args$filters <- NULL
  selected_value <- adapt_rules_to_limits(
    filter@value,
    filters,
    reset
  )

  params <- modify_list(
    list(
      inputId = input_id,
      rules = selected_value,
      filters = filters,
      allow_add_rules = TRUE,
      allow_groups = TRUE
    ),
    gui_args
  )

  return(params)
}

S7::method(.gui_filter, cohortBuilder::CbFilterQuery) <- function(object, ...) {
  if (!requireNamespace("shinyQueryBuilder", quietly = TRUE)) {
    stop("In order to use 'query' filter, please install 'shinyQueryBuilder' package.")
  }
  list(
    input = function(filter, input_id, cohort) {
      input_params <- query_input_params(filter, input_id, cohort, ...)
      input_params$inputId <- paste0(input_id, "_selected")
      modal_dialog_id <- paste0(input_id, "modal_in")
      move_dialog_to_body_js <- move_modal_dialog_js(modal_dialog_id, input_id, "body")
      move_dialog_back_js <- move_modal_dialog_js(modal_dialog_id, input_id, "container")

      shiny::tagList(
        shinyGizmo::modalDialogUI(
          modal_dialog_id,
          do.call(shinyQueryBuilder::queryBuilderInput, input_params),
          backdrop = FALSE,
          size = "l",
          footer = shiny::tagList(
            .cb_input(
              rlang::inject(shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("#", input_params$inputId),
                !!!bs_data_attr("dismiss", "modal"),
                onclick = move_dialog_back_js
              )),
              filter@private$input_param,
              style = "display: inline-block;"
            ),
            shiny::modalButton("Dismiss") |>
              htmltools::tagAppendAttributes(
                onclick = move_dialog_back_js
              )
          ),
          button = button(
            getOption("scb_labels", scb_labels)$filter_query_bttn_label,
            icon = getOption("scb_icons", scb_icons)$filter_query_bttn_icon,
            class = "btn-sm scb-input-button",
            !!!bs_data_attr("toggle", "modal"), !!!bs_data_attr("target", paste0("#", modal_dialog_id)),
            onclick = move_dialog_to_body_js
          )
        ),
        .cb_input(
          .keep_na_input(input_id, filter, cohort, msg_fun = function(x) "Keep missing values"),
          "keep_na"
        )
      )
    },
    feedback = function(filter, input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot") ,
        output_fun = shiny::htmlOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderUI({
            if(empty) { # when no data in parent step
              return(NULL)
            }
            ns <- cohort$attributes$session$ns
            filter_val <- queryBuilder::queryToExpr(filter@value)
            modal_dialog_id <- shiny::NS(ns(input_id), "query_modal")
            plot_id <- shiny::NS(ns(input_id), "feedback_plot")
            move_dialog_to_body_js <- move_modal_dialog_js(modal_dialog_id, ns(input_id), "body")
            move_dialog_back_js <- move_modal_dialog_js(modal_dialog_id, ns(input_id), paste0("#", plot_id))
            return(
              shinyGizmo::modalDialogUI(
                modal_dialog_id,
                htmltools::pre(htmltools::code(utils::capture.output(filter_val), .noWS = no_ws), .noWS = no_ws),
                backdrop = FALSE,
                size = "l",
                button = button(
                  getOption("scb_labels", scb_labels)$filter_show_query_bttn_label,
                  icon = getOption("scb_icons", scb_icons)$filter_show_query_bttn_icon,
                  class = "btn-sm scb-input-button",
                  !!!bs_data_attr("toggle", "modal"), !!!bs_data_attr("target", paste0("#", modal_dialog_id)),
                  onclick = move_dialog_to_body_js
                ),
                footer = shiny::modalButton("Dismiss") |>
                  htmltools::tagAppendAttributes(onclick = move_dialog_back_js)
              )
            )
          })
        }
      )
    },
    server = function(filter, input_id, input, output, session, cohort) {},
    update = function(filter, session, input_id, cohort, reset = FALSE, ...) {
      update_params <- query_input_params(filter, input_id, cohort, reset, TRUE, ...)
      update_params$inputId <- paste0(input_id, "_selected")

      do.call(
        shinyQueryBuilder::updateQueryBuilderInput,
        append(
          list(session = session),
          update_params
        )
      )
    },
    post_stats = FALSE,
    multi_input = FALSE
  )
}
