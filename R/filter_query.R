take_from_list <- function(list_obj, id) {
  purrr::keep(list_obj, ~ .$id == id)[[1]]
}

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

adapt_rules_vals_to_limits <- function(rules, filters, reset) {

  if (!is.null(rules$condition)) {
    rules$rules <- purrr::modify(rules$rules, adapt_rules_vals_to_limits, filters = filters, reset = reset)
  } else {
    rules$value <- adapt_vals_to_limits(rules, filters, reset)
  }

  return(rules)
}

adapt_rules_to_limits <- function(rules, filters, reset = FALSE) {

  if (identical(rules, NA) || identical(rules, NULL)) {
    return(list())
  }
  rules <- adapt_rules_vals_to_limits(rules, filters, reset)

  return(rules)
}

query_input_params <- function(filter, input_id, cohort, reset = FALSE, update = FALSE, ...) {

  step_id <- filter$step_id
  filter_id <- filter$id

  if (!cohort$get_cache(step_id, filter_id, state = "pre")$n_data) {
    return(
      list(inputId = input_id)
    )
  }

  gui_args <- list(...)
  if (is.null(gui_args$filters)) {
    gui_args$filters <- list()
  }

  parent_specs <- cohort$get_cache(step_id, filter_id, state = "pre")$specs
  setting_from_stat <- base::get("setting_from_stat", envir = asNamespace("shinyQueryBuilder"), inherits = FALSE)
  filters <- filter$get_params("variables") %>%
    purrr::map(
      ~ setting_from_stat(
        parent_specs[[.x]], .x, !!!gui_args$filters[[.x]],
        .queryBuilderConfig = queryBuilder::queryBuilderConfig
      )
    ) |>
    purrr::map(~rlang::inject(shinyQueryBuilder::queryFilter(!!!.x)))
  gui_args$filters <- NULL
  selected_value <- adapt_rules_to_limits(
    filter$get_params("value"),
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

#' @rdname gui-filter-layer
#' @export
.gui_filter.query <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_params <- query_input_params(filter, input_id, cohort, ...)
      input_params$inputId <- paste0(input_id, "_selected")

      shiny::tagList(
        shinyGizmo::modalDialogUI(
          paste0(input_id, "modal_in"),
          do.call(shinyQueryBuilder::queryBuilderInput, input_params),
          backdrop = FALSE,
          size = "l",
          footer = shiny::tagList(
            .cb_input(
              shinyGizmo::valueButton(
                inputId = input_id,
                label = "Accept",
                selector = paste0("#", input_params$inputId),
                `data-dismiss` = "modal", `data-bs-dismiss` = "modal"
              ),
              filter$input_param,
              style = "display: inline-block;"
            ),
            shiny::modalButton("Dismiss")
          ),
          button = button(
            "Set rules",
            icon = shiny::icon("keyboard"), style = "width: 100%; margin-top: 0.5em; margin-bottom: 0.5em;",
            `data-toggle` = "modal", `data-target` = paste0("#", paste0(input_id, "modal_in")),
            `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", paste0(input_id, "modal_in"))
          )
        ),
        .cb_input(
          .keep_na_input(input_id, filter, cohort, msg_fun = function(x) "Keep missing values"),
          "keep_na"
        )
      )
    },
    feedback = function(input_id, cohort, empty = FALSE) {
      list(
        plot_id = shiny::NS(input_id, "feedback_plot") ,
        output_fun = shiny::htmlOutput,
        render_fun = if (!is.null(empty)) {
          shiny::renderUI({
            if(empty) { # when no data in parent step
              return(NULL)
            }
            filter_val <- queryBuilder::queryToExpr(filter$get_params("value"))
            modal_id <- shiny::NS(input_id, "query_modal")
            return(
              shinyGizmo::modalDialogUI(
                modal_id,
                htmltools::pre(htmltools::code(utils::capture.output(filter_val))),
                button = button(
                  "Show query",
                  icon = shiny::icon("keyboard"), style = "width: 100%; margin-top: 0.5em; margin-bottom: 0.5em;",
                  `data-toggle` = "modal", `data-target` = paste0("#", modal_id),
                  `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", modal_id)
                )
              )
            )
          })
        }
      )
    },
    server = function(input_id, input, output, session, cohort) {},
    update = function(session, input_id, cohort, reset = FALSE, ...) {
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
