pkgload::load_all("../cohortBuilder")

# -- Setup source with descriptions -------------------------------------------

dt_source <- set_source(
  tblist(
    iris = iris |> dplyr::mutate(spec_id = paste("spec", 1:dplyr::n())),
    mtcars = mtcars
  ),
  # `label` sets a short, human-readable filter name (shown in the filter
  # picker), while the description text carries the longer explanation.
  description = list(
    iris = list(
      dataset_ = describe("dataset related to iris plants"),
      spec_id = describe("unique row id", label = "Specimen ID"),
      Sepal.Length = describe("the sepal length measurement", label = "Sepal length"),
      Petal.Length = describe("the petal length measurement", label = "Petal length"),
      Sepal.Width = describe("the sepal width measurement", label = "Sepal width"),
      Petal.Width = describe("the petal width measurement", label = "Petal width"),
      Species = describe("the species of iris", label = "Species")
    ),
    mtcars = list(
      dataset_ = describe("dataset related to car specifications"),
      mpg = describe("Miles/(US) gallon", label = "Mileage"),
      cyl = describe("Number of cylinders", label = "Cylinders"),
      disp = describe("Displacement (cu.in.)", label = "Displacement"),
      hp = describe("Gross horsepower", label = "Horsepower"),
      drat = describe("Rear axle ratio", label = "Rear axle ratio"),
      wt = describe("Weight (1000 lbs)", label = "Weight"),
      qsec = describe("1/4 mile time", label = "Quarter-mile time"),
      vs = describe("Engine (0 = V-shaped, 1 = straight)", label = "Engine shape"),
      am = describe("Transmission (0 = automatic, 1 = manual)", label = "Transmission"),
      gear = describe("Number of forward gears", label = "Gears"),
      carb = describe("Number of carburetors", label = "Carburetors")
    )
  )
) |> autofilter(attach_as = "meta")

coh <- cohort(source = dt_source)

# -- Register tools with chat -------------------------------------------------

chat <- ellmer::chat_azure_openai(
  endpoint = Sys.getenv("CHAT_ENDPOINT"),
  model = "gpt-4o",
  api_version = "2024-08-01-preview",
  system_prompt = "You are a helpful assistant.",
  credentials = function() list("api-key" = Sys.getenv("CHAT_KEY")),
  echo = "all"#,
  #api_args = list(parallel_tool_calls = FALSE)
)

chat |> cb_register_tools(coh)

# -- Shiny app with assistant --------------------------------------------------

library(shiny)
pkgload::load_all()

shiny::runApp(
  list(
    ui = bslib::page_sidebar(
      title = "AI Assistant",
      sidebar = bslib::sidebar(
        shinyCohortBuilder::cb_ui(id = "data", assistant = TRUE, new_step = "configure")
      ),
      bslib::card(
        shiny::verbatimTextOutput("data_obj")
      )
    ),
    server = function(input, output, session) {
      shinyCohortBuilder::cb_server(
        id = "data", coh, run_button = "none",
        feedback = TRUE, chat = chat
      )

      returned_data <- shiny::eventReactive(input[["data-cb_data_updated"]], {
        coh$get_data(step_id = coh$last_step_id(), state = "post")
      }, ignoreInit = FALSE, ignoreNULL = FALSE)

      output$data_obj <- shiny::renderPrint({
        print(returned_data())
      })
    }
  ),
  host = "0.0.0.0",
  port = 8888,
  launch.browser = TRUE
)
