take_from_list <- function(list_obj, id) {
  purrr::keep(list_obj, ~ .$id == id)[[1]]
}

rematch_value <- function(val, val_set, stat_name) {
  if (stat_name == "values") {
    return(intersect(val, val_set))
  }
  if (stat_name == "validation") {
    # todo extend to not only between but other operators
    if (length(val) == 2) {
      return(c(max(val[1], val_set[1]), min(val[2], val_set[2])))
    }
    return(val)
  }
  return(val)
}

limit_vals <- function(rule, filters, reset) {
  filter_vals <- take_from_list(filters, rule$id)
  stat_name <- filter_vals$stat_name
  filter_vals <- unlist(unname(filter_vals[[stat_name]]))

  if (identical(rule$value, NA) || reset) {
    rule$value <- filter_vals
  } else {
    rule$value <- rematch_value(rule$value, filter_vals, stat_name)
  }

  return(rule$value)
}

customize_vals <- function(rules, filters, reset) {

  if (!is.null(rules$condition)) {
    rules$rules <- purrr::modify(rules$rules, customize_vals, filters = filters, reset = reset)
  } else {
    rules$value <- limit_vals(rules, filters, reset)
  }

  return(rules)
}

customize_rules <- function(rules, filters, reset = FALSE) {

  if (identical(rules, NA) || identical(rules, NULL)) {
    return(list())
  }
  rules <- customize_vals(rules, filters, reset)

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

  parent_specs <- cohort$get_cache(step_id, filter_id, state = "pre")$specs
  filters <- filter$get_params("variables") %>%
    purrr::map(~shinyQueryBuilder:::setting_from_stat(parent_specs[[.x]], .x))
  selected_value <- customize_rules(
    filter$get_params("value"),
    filters,
    reset
  )

  params <- list(
    inputId = input_id,
    rules = selected_value,
    filters = filters,
    allow_new_rules = TRUE,
    allow_groups = TRUE,
    ...
  )

  return(params)
}

#' @rdname gui-filter-layer
#' @export
.gui_filter.query <- function(filter, ...) {
  list(
    input = function(input_id, cohort) {
      input_params <- query_input_params(filter, input_id, cohort)
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
            `data-toggle` = "modal", `data-target` = paste0("#", paste0(input_id, "modal_in"))
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
            return(
              ggiraph::girafe(
                ggobj      = ggplot2::ggplot(),
                width_svg  = 10,
                height_svg = 0.1
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
