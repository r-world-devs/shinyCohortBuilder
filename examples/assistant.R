pkgload::load_all("../cohortBuilder")

# -- Setup source with descriptions -------------------------------------------

dt_source <- set_source(
  tblist(
    iris = iris,
    mtcars = mtcars
  ),
  description = list(
    iris = list(
      dataset_ = describe("dataset related to iris plants"),
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
  credentials = function() list("api-key" = Sys.getenv("CHAT_KEY"))
)

chat |> cb_register_tools(coh)

# -- Shiny app with assistant --------------------------------------------------

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
