pkgload::load_all("../cohortBuilder")
describe <- function(description, ...) {
  # already_described <- is.list(description) && !is.null(description$text)
  # if (already_described) {
  #   return(already_description)
  # }
  list(
    text = description,
    ...
  )
}

# extract_choices <- function(x, filter_type) {
#
# }

dt_source <- set_source(
  tblist(
    iris = iris,
    mtcars = mtcars
  ),
  description = list(
    iris = list(
      dataset_ = describe("dataset related to iris plants"),
      Sepal.Length = describe("filter for the sepal length measurement", stats = c("never", "auto")),
      Petal.Length = describe("filter for the petal length measurement"),
      Sepal.Width = describe("filter for the sepal width measurement"),
      Petal.Width = describe("filter for the petal width measurement"),
      Species = describe("filter for the species of iris")
    ),
    mtcars = list(
      dataset_ = describe("dataset related to car specifications"),
      mpg =	describe("Miles/(US) gallon"),
      cyl =	describe("Number of cylinders"),
      disp =	describe("Displacement (cu.in.)"),
      hp =	describe("Gross horsepower"),
      drat =	describe("Rear axle ratio"),
      wt =	describe("Weight (1000 lbs)"),
      qsec =	describe("1/4 mile time"),
      vs =	describe("Engine (0 = V-shaped, 1 = straight)"),
      am =	describe("Transmission (0 = automatic, 1 = manual)"),
      gear =	describe("Number of forward gears"),
      carb =	describe("Number of carburetors")
    )
  )
) |> cohortBuilder::autofilter(attach_as = "meta")

coh <- cohort(
  source = dt_source
)

chat <- ellmer::chat_azure_openai(
  endpoint = Sys.getenv("CHAT_ENDPOINT"),
  params = ellmer::params(seed = 2),
  deployment_id = "gpt-4o",
  api_version = "2024-08-01-preview",
  system_prompt = "You are a helpful assistant.",
  credentials = list("api-key" = Sys.getenv("CHAT_KEY"))
)

set_chat_tool <- function(chat, cb_tool, description, ...) {
  if (missing(description)) {
    description <- attr(cb_tool, "description")
  }
  args <- list(...)
  if (length(args) == 0) {
    args <- attr(cb_tool, "params")
  }
  chat$register_tool(
    rlang::inject(
      ellmer::tool(
        fun = cb_tool,
        description = description,
        !!!args
      )
    )
  )
  return(chat)
}

get_filters_meta_tool <- function(cohort) {
  fun <- function() {
    filters_meta <- shape(cohort$get_source())
    return(jsonlite::toJSON(filters_meta, auto_unbox = TRUE))
  }
  attr(fun, "description") <- r"(
    The tool returns information of available filters in json format.
    The json is a set of objects, each object describing either dataset (when filter field is not specified) or filter description (otherwise).
    Fields named 'filter' are storing the filter id.
    Fields named 'dataset' are storing the dataset name that filter is attached to.
    Fields named 'desciption' are storing the description of filter purpose.
    Fields named 'stats' are storing related filter limits:
      - 'choices' lists available options,
      - 'range' provides numerical values the filter should operate within.
  )"
  attr(fun, "params") <- list(
    name = "get_filters_meta"
  )
  return(fun)
}

set_chat_tool(chat, get_filters_meta_tool(coh))

#chat$chat("Get information about available filters.")
#chat$chat("What are the filters in mtcars dataset?")

#sum_up(coh)

# Tool description:
# ellmer's tools (is it possible to keep cohort as argument)
add_filters_tool <- function(cohort, action = c("edit_last", "new_step"), ...) {
  action <- match.arg(action, several.ok = TRUE)
  fun <- function(filter_ids, action = action) {
    print("add_filters")
    print(filter_ids)
    filter_ids <- strsplit(filter_ids, ",")[[1]]
    action <- match.arg(action, several.ok = FALSE)
    data_source <- cohort$get_source()
    available_filters <- data_source$available_filters
    filters_to_set <- available_filters %>%
      purrr::map(~.x(data_source)) |>
      purrr::keep(function(x) {x$name %in% filter_ids})
    # if (action == "edit_last") {
    #   cohort$edit_step(filters, step_id = last_step())
    # }
    if (action == "new_step") {
      cohort$copy_step(
        filters = filters_to_set,
        run_flow = FALSE
      )
    }
    msg <- glue::glue("The following filters have been set: {paste(filter_ids, collapse = ', ')}")
    return(msg)
  }
  attr(fun, "description") <- r"(
    The tool used to set specific set of filters to a new filtering step.
    Available filters can be extracted using 'get_filters_meta' tool.
    Very important: The tool should be called once for all the filter ids of user interest.
  )"
  attr(fun, "params") <- list(
    name = "add_filters",
    filter_ids = ellmer::type_string(
      "Comma separated filter ids that should be set to the cohort."
    ),
    action = ellmer::type_string(
      "Always equal to 'new_step' string."
    )
  )
  return(fun)
}

# Mandatory assumptions (follow them no matter what):
#   - The tool should be called for all the filters ids of user interest at once (even if they're linked to different datasets).
#       - Never set the same set of filters multiple times.
#       - If you encounter an error, don't try to call the tool again.

set_chat_tool(chat, add_filters_tool(coh))

# sum_up(coh)
#
# chat$chat("Set filters that will allow me to specify iris species and a car speed.")
#
# sum_up(coh)

set_filter_values_tool <- function(cohort, ...) {
  fun <- function(filter_values) {
    print("set_filters")
    print(filter_values)
    filter_vals <- jsonlite::fromJSON(filter_values)
    for (filter_id in names(filter_vals)) {
      print(filter_vals[[filter_id]])
      do.call(
        cohort$update_filter,
        append(
          list(
            step_id = cohort$last_step_id(), filter_id = filter_id,
            hook_args = list(pre = list(), post = list(update_active = FALSE, update = "input"))
          ),
          filter_vals[[filter_id]]
        )
      )
    }
    run(cohort)
    msg <- glue::glue("The following filter values have been updated: {paste(capture.output(str(filter_vals)), collapse = ', ')}")
    return(msg)
  }
  attr(fun, "description") <- r"(
    The tool used to set filter values.
    Available filters domain can be extracted using 'get_filters_meta' tool and are stored within stats field.
  )"
  attr(fun, "params") <- list(
    name = "set_filter_values",
    filter_values = ellmer::type_string(
      "JSON object storing filter values to be set.
      Each element should be named as filter id and store the following elements:
      - 'value' - array of desired values for discrete-type filter.
      - 'range' - array of two values - minimal and maximal value to be set for range-type filter."
    )
  )
  return(fun)
}

set_chat_tool(chat, set_filter_values_tool(coh))

#chat$chat("Filter iris species that start with 'v' letter and cars having horse power above 100.")

library(shiny)
pkgload::load_all()

shiny::runApp(list(
  ui = bslib::page_sidebar(
    title = "AI Assistant",
    sidebar = bslib::sidebar(
      shinyCohortBuilder::cb_ui(id = "data", assistant = TRUE)
    ),
    bslib::card(
      shiny::verbatimTextOutput("data_obj")
    )
  ),
  server = function(input, output, session) {

    shinyCohortBuilder::cb_server(id = "data", coh, run_button = "global", feedback = TRUE)
    cb_chat_server("data-chat", chat, input, output, session)

    returned_data <- shiny::eventReactive(input[["data-cb_data_updated"]], {
      coh$get_data(step_id = coh$last_step_id(), state = "post")
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    output$data_obj <- shiny::renderPrint({
      print(returned_data())
    })
  }
))
