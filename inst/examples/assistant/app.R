pkgload::load_all("../cohortBuilder")

# -- Setup source with descriptions -------------------------------------------

dt_source <- set_source(
  tblist(
    iris = iris |> dplyr::mutate(spec_id = paste("spec", 1:dplyr::n())),
    mtcars = mtcars
  ),
  description = list(
    iris = list(
      dataset_ = describe("dataset related to iris plants"),
      spec_id = describe("unique row id"),
      Sepal.Length = describe("filter for the sepal length measurement"),
      Petal.Length = describe("filter for the petal length measurement"),
      Sepal.Width = describe("filter for the sepal width measurement"),
      Petal.Width = describe("filter for the petal width measurement"),
      Species = describe("filter for the species of iris")
    ),
    mtcars = list(
      dataset_ = describe("dataset related to car specifications"),
      mpg = describe("Miles/(US) gallon"),
      cyl = describe("Number of cylinders"),
      disp = describe("Displacement (cu.in.)"),
      hp = describe("Gross horsepower"),
      drat = describe("Rear axle ratio"),
      wt = describe("Weight (1000 lbs)"),
      qsec = describe("1/4 mile time"),
      vs = describe("Engine (0 = V-shaped, 1 = straight)"),
      am = describe("Transmission (0 = automatic, 1 = manual)"),
      gear = describe("Number of forward gears"),
      carb = describe("Number of carburetors")
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
