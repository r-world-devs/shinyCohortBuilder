#' Run demo application
#'
#' The demo presents available filters and toolbox features.
#'
#' @param steps Set to TRUE (default) if multiple steps should be available.
#' @param stats Choose which statistics should be displayed for data (and some filters).
#'   Possible options are: "pre" - previous step stat, "post" - current step stats,
#'   `c("pre", "post")` - for both and NULL for no stats.
#' @param run_button Should Run button be displayed? If so, the current step computations are run only when clicked.
#'   Three options are available "none" - no button, "local" - button displayed at each step panel,
#'   "global" - button visible in top filtering panel.
#' @param feedback Set to TRUE (default) if feedback plots should be displayed at each filter.
#' @param state Set to TRUE (default) to enable get/set state panel.
#' @param bootstrap Boostrap version to be used for filtering panel.
#'   See \link[bslib]{bs_theme} version argument.
#' @param enable_bookmarking Set to TRUE (default) if panel should be compatible with native shiny bookmarking.
#' @param code Set to TRUE (default) to enable reproducible code panel.
#' @param attrition Set to TRUE (default) to enable attrition plot panel.
#' @param show_help Set to TRUE (default) to enable help buttons.
#' @param new_step Choose which add step method should be used for creating new step.
#'   Possible options are: "clone" - copy filters from last step,
#'   "configure" - opening modal and allow to chose filters from available filters.
#' @param ... Extra parameters passed to selected cohort methods.
#'   Currently unused.
#' @param run_app If 'TRUE' the application will run using \link[shiny]{runApp},
#'   otherwise \link[shiny]{shinyApp} object is returned.
#' @return In case of `run_app=TRUE` no return value, used for side effect which is running a Shiny application.
#'   Otherwise \link[shiny]{shinyApp} object.
#'
#' @examples
#' if (interactive()) {
#'   library(shinyCohortBuilder)
#'   demo_app(steps = FALSE)
#' }
#' if (interactive()) {
#'   library(shinyCohortBuilder)
#'   demo_app(run_button = "local", state = FALSE)
#' }
#' @export
demo_app <- function(
  steps = TRUE, stats = c("pre", "post"), run_button = "none", feedback = TRUE, state = TRUE,
  bootstrap = 5, enable_bookmarking = TRUE, code = TRUE, attrition = TRUE, show_help = TRUE,
  new_step = c("clone", "configure"), ..., run_app = TRUE) {

  if (is.logical(run_button)) {
    lifecycle::deprecate_stop("0.2.0", "shinyCohorBuilder::demo_app(arg = 'must be a scalar character')")
  }
  run_method <- shiny::shinyApp
  if (isTRUE(run_app)) {
    run_method <- function(ui, server) shiny::runApp(list(ui = ui, server = server), ...)
  }

  old_opts <- options()
  on.exit(options(old_opts))
  options("cb_active_filter" = FALSE)
  if (isTRUE(enable_bookmarking)) {
    shiny::enableBookmarking(store = "url")
  } else {
    shiny::enableBookmarking(store = "disable")
  }
  gender_mapping <- function(values, cohort) {
    unname(c("F" = "Female", "M" = "Male")[values])
  }
  datasets <- list(
    "01" = cohortBuilder::tblist(
      patients = data.frame(
        id = 1:10,
        group = c("A", "B", "C", "B", "B", "C", "A", "B", "C", "B"),
        gender = c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M"),
        age = c(50L, 44L, 38L, 49L, 45L, 33L, 43L, 35L, 40L, NA),
        visit = as.Date(c(7152, 7578, 7639, 7121, 7395, 7425, 7456, 7517, 6971, 7030), origin = "1970-01-01"),
        biom1 = c("A", "B", "A", "A", "B", "B", "B", "A", NA, "A"),
        biom2 = c("C", "D", "C", "D", "E", "E", "C", "C", "E", "C")
      ),
      therapy = data.frame(
        id = 1:10,
        treatment = c("Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Atezo", "Atezo")
      )
    ),
    "02" = cohortBuilder::tblist(
      patients = data.frame(
        id = 1:15,
        group = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "B", "D", "D", "D", "D", "D"),
        gender = c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M", "F", "M", "F", "M", "F"),
        age = c(42L, 34L, 48L, 43L, 32L, 37L, 47L, 41L, 38L, 46L, 36L, 44L, 40L, 35L, 31L),
        visit = as.Date(c(7364, 7548, 7060, 7152, 7486, 7213, 7456, 7517, 7274, 6971, 7639, 7091, 7425, 7030, 6940), origin = "1970-01-01"),
        biom1 = c("A", "B", "A", "A", "B", "B", "B", "A", "A", "A", "B", "A", "A", "B", "B"),
        biom2 = c("C", "D", "C", "D", "E", "E", "C", "C", "E", "C", "C", "D", "C", "D", "E"),
        biom3 = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "B")
      ),
      therapy = data.frame(
        patient_id = c(1:15, 1:5, 1:3),
        line_id = c(rep(1, 15), rep(2, 5), rep(3, 3)),
        treatment = c(
          "Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Nebul", "Atezo", "Chemo", "Atezo", "Atezo", "Atezo", "Nebul",
          "Nebul", "Atezo", "Chemo", "Atezo", "Atezo", "Nebul", "Atezo", "Chemo", "Nebul", "Atezo", "Atezo"
        )
      ) %>%
        dplyr::mutate(id = paste(patient_id, line_id, sep = "_")) %>%
        dplyr::relocate(id, .before = "patient_id")
    )
  )

  run_method(
    ui = function(req) {
      bslib::page_fluid(
        theme = bslib::bs_theme(version = bootstrap),
        if (isTRUE(enable_bookmarking)) shiny::bookmarkButton() else NULL,
        shiny::radioButtons("dataset", "Source", c("No binding keys" = "01", "Binding keys" = "02")),
        cb_ui(
          id = "ptnts", style = "width: 300px; float: left;", steps = steps,
          state = state, code = code, attrition = attrition, new_step = new_step
        ),
        shiny::div(style = "float: right; width: calc(100% - 300px);",
          shiny::verbatimTextOutput("datasets")
        )
      )
    }, server = function(input, output, session) {
      # Adam's idea: create in global and just clone in session

      #group_filter <- cohortBuilder::filter(type = "discrete", id = "group", name = "Group", variable = "group", dataset = "patients")
      group_filter <- cohortBuilder::filter(
        type = "discrete_text", id = "group", name = "Group", variable = "group", dataset = "patients"
      )
      gender_filter <- cohortBuilder::filter(
        type = "discrete", id = "gender", name = "Gender", variable = "gender",
        dataset = "patients", value = "M", value_mapping = "gender_mapping",
        description = "The Gender field denotes the biological sex at birth.",
        stats = "pre"
      )
      age_filter <- cohortBuilder::filter(
        type = "range", id = "age", name = "Age", variable = "age", dataset = "patients", range = NA,
        description = "The Age field is an length of time that a person has lived or a thing has existed.",
        feedback = FALSE
      )
      treatment_filter = list(
        "01" = cohortBuilder::filter(
          type = "discrete", id = "treatment", name = "Treatment", variable = "treatment",
          dataset = "therapy", value = "Atezo", gui_input = "vs", stats = "post"
        ),
        "02" = cohortBuilder::filter(
          type = "query", id = "treatment", name = "Treatment",
          variables = c("treatment", "line_id"), dataset = "therapy",
          value = queryBuilder::queryGroup(
            condition = "AND",
            queryBuilder::queryRule("treatment", "equal", "Atezo"),
            queryBuilder::queryRule("line_id", "less_or_equal", 3)
          )
        )
      )
      visit_filter <- cohortBuilder::filter(
        "date_range", name = "Visit", variable = "visit", dataset = "patients"
      )
      biom_filter <- cohortBuilder::filter(
        type = "multi_discrete", id = "bioms", name = "Biomarkers", dataset = "patients",
        values = NULL, variables = c("biom1", "biom2")
      )

      binding_keys <- list(
        "01" = NULL,
        "02" = cohortBuilder::bind_keys(
          cohortBuilder::bind_key(
            update = cohortBuilder::data_key("therapy", "patient_id"),
            cohortBuilder::data_key("patients", "id"),
            post = FALSE
          ),
          cohortBuilder::bind_key(
            update = cohortBuilder::data_key("patients", "id"),
            cohortBuilder::data_key("therapy", "patient_id"),
            post = FALSE
          )
        )
      )

      patients_source <- cohortBuilder::set_source(
        datasets[["01"]],
        value_mappings = list(
          gender_mapping = gender_mapping
        ),
        description = list(
          patients = "Contains demographic information about patients."
        ),
        available_filters = list(
          gender_filter,
          age_filter,
          treatment_filter[["01"]]
        ),
        primary_keys = cohortBuilder::primary_keys(
          cohortBuilder::data_key("patients", "id"),
          cohortBuilder::data_key("therapy", "id")
        ),
        source_code = substitute({
            source <- list(attributes = list(datasets = datasets_value))
          },
          list(
            datasets_value = dput(datasets[["01"]], file = tempfile())
          )
        )
      )

      url_state <- shiny::isolate(
        shiny::parseQueryString(session$clientData$url_search)$state
      )

      if (!is.null(url_state)) {
        coh <- cohortBuilder::cohort(
          patients_source
        )
        coh$restore(url_state)
      } else {

        coh <- cohortBuilder::cohort(
          patients_source,
          cohortBuilder::step(
            group_filter,
            gender_filter,
            age_filter,
            treatment_filter[["01"]],
            visit_filter,
            biom_filter
          )
        )
      }

      cb_server(
        id = "ptnts", coh, run_button = run_button, stats = stats, feedback = feedback,
        enable_bookmarking = enable_bookmarking, show_help = show_help
      ) # todo stats = NULL (no stats and maybe? no prots, then some optimization)

      shiny::observeEvent(input$dataset, {
        data_source <- cohortBuilder::set_source(
          datasets[[input$dataset]],
          available_filters = list(
            gender_filter,
            age_filter,
            treatment_filter[[input$dataset]]
          ),
          value_mappings = list(
            gender_mapping = gender_mapping
          ),
          binding_keys = binding_keys[[input$dataset]],
          primary_keys = cohortBuilder::primary_keys(
            cohortBuilder::data_key("patients", "id"),
            cohortBuilder::data_key("therapy", "id")
          ),
          source_code = substitute({
              source <- list(attributes = list(datasets = datasets_value))
            },
            list(
              datasets_value = datasets[[input$dataset]]
            )
          )
        ) %>%
          cohortBuilder::add_step(
            cohortBuilder::step(
              group_filter,
              gender_filter,
              age_filter,
              treatment_filter[[input$dataset]],
              visit_filter
            )
          )
        # three options available
        # 1. keep_steps = TRUE preserves all the steps with the selected values.
        # 2. keep_steps = c(1L) or more, the only defined are preserved but also cleared (preferred option when only data changes and we want to keep first step definition)
        # 3. keep_step = FALSE - the logic assumes the source have steps provided and renders them.
        coh$update_source(data_source)
      }, ignoreInit = TRUE)

      returned_data <- shiny::eventReactive(input[["ptnts-cb_data_updated"]], {
        coh$get_data(step_id = coh$last_step_id(), state = "post")
      }, ignoreInit = FALSE, ignoreNULL = FALSE)

      output$datasets <- shiny::renderPrint({
        print(returned_data())
      })
    }
  )
}

# todo Make queue of actions and with delay execute them.

#' Run filtering panel locally
#'
#' @inheritParams demo_app
#' @param cohort Cohort object with configured filters.
#' @return No return value, used for side effect which is running a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   library(magrittr)
#'   library(cohortBuilder)
#'   library(shinyCohortBuilder)
#'   mtcars_source <- set_source(tblist(mtcars = mtcars))
#'   mtcars_cohort <- cohort(
#'     mtcars_source,
#'     filter("discrete", id = "am", dataset = "mtcars", variable = "am", value = 1)
#'   ) %>% run()
#'   gui(mtcars_cohort)
#' }
#'
#' @export
gui <- function(
  cohort,
  steps = TRUE, stats = c("pre", "post"), run_button = "none", feedback = TRUE, state = TRUE,
  bootstrap = 5, enable_bookmarking = TRUE, code = TRUE, attrition = TRUE, show_help = TRUE,
  new_step = c("clone", "configure")) {

  if (is.logical(run_button)) {
    lifecycle::deprecate_stop("0.2.0", "shinyCohorBuilder::gui(arg = 'must be a scalar character')")
  }

  if (!interactive()) {
    stop("Message - gui can be used in interactive mode only.")
  }
  new_step <- rlang::arg_match(new_step)
  if (identical(new_step, "configure") && length(cohort$get_source()$get("available_filters")) == 0) {
    stop("The `available_filters` in the cohort source wasn't defined.")
  }

  shiny::runApp(list(
    ui = function(req) {
      bslib::page_fluid(
        theme = bslib::bs_theme(version = bootstrap),
        cb_ui(
          id = "coh", style = "width: 300px; float: left;",
          steps = steps, state = state, code = code, attrition = attrition,
          new_step = new_step
        ),
        shiny::div(
          style = "float: right; width: calc(100% - 300px);",
          shiny::verbatimTextOutput("datasets")
        )
      )
    }, server = function(input, output, session) {
      cb_server(
        id = "coh", cohort, run_button = run_button, stats = stats, feedback = feedback,
        enable_bookmarking = enable_bookmarking, show_help = show_help
      )
      returned_data <- shiny::eventReactive(input[["coh-cb_data_updated"]], {
        cohort$get_data(state = "post")
      }, ignoreInit = FALSE, ignoreNULL = FALSE)
      output$datasets <- shiny::renderPrint({
        print(returned_data())
      })
    }
  ))
}
