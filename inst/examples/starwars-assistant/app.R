# Star Wars AI assistant example
#
# A cohortBuilder + shinyCohortBuilder app over four related Star Wars tables
# (people, planets, species, films) with an LLM assistant mounted in a right
# sidebar. The assistant inspects and applies filters through cohortBuilder's
# registered tools; the filter panel, the assistant, and the data tables all
# operate on the same cohort.
#
# The assistant reads all config from environment variables:
#   ANTHROPIC_BASE_URL, ANTHROPIC_AUTH_TOKEN, ANTHROPIC_CUSTOM_HEADERS,
#   ANTHROPIC_MODEL (optional), STARWARS_APP_PORT (optional, default 3838)
#
# Run:
#   Rscript app.R
#   # or shiny::runApp("inst/examples/starwars-assistant")

# The packages target R >= 4.4 where `%||%` is in base. On older R, inject it
# into their (locked) namespaces so package internals resolve the operator.
if (getRversion() < "4.4.0") {
  for (pkg in c("cohortBuilder", "shinyCohortBuilder")) {
    requireNamespace(pkg, quietly = TRUE)
    ns <- asNamespace(pkg)
    if (!exists("%||%", envir = ns, inherits = FALSE)) {
      rlang::env_unlock(ns)
      assign("%||%", rlang::`%||%`, envir = ns)
      lockEnvironment(ns, bindings = FALSE)
    }
  }
}

library(cohortBuilder)
library(shinyCohortBuilder)
library(shiny)
options(shiny.fullstacktrace = TRUE, shiny.trace = FALSE, warn = 1)

# ANTHROPIC_CUSTOM_HEADERS holds one "Key: value" pair; split on the first colon.
parse_custom_headers <- function(x) {
  if (!nzchar(x)) return(character(0))
  idx <- regexpr(":", x, fixed = TRUE)
  if (idx < 1) return(character(0))
  stats::setNames(trimws(substr(x, idx + 1, nchar(x))),
                  trimws(substr(x, 1, idx - 1)))
}

build_chat <- function() {
  base_url <- Sys.getenv("ANTHROPIC_BASE_URL")
  token    <- Sys.getenv("ANTHROPIC_AUTH_TOKEN")
  if (!nzchar(base_url) || !nzchar(token)) {
    stop("ANTHROPIC_BASE_URL and ANTHROPIC_AUTH_TOKEN must be set.", call. = FALSE)
  }
  # ellmer::chat_azure_openai(
  #   endpoint = Sys.getenv("CHAT_ENDPOINT"),
  #   model = "gpt-4o",
  #   api_version = "2024-08-01-preview",
  #   system_prompt = "You are a helpful assistant.",
  #   credentials = function() list("api-key" = Sys.getenv("CHAT_KEY")),
  #   echo = "all"#,
  #   #api_args = list(parallel_tool_calls = FALSE)
  # )
  ellmer::chat_anthropic(
    # ellmer appends "/messages"; Anthropic endpoints expect the "/v1" prefix.
    base_url = paste0(sub("/+$", "", base_url), "/v1"),
    credentials = function() token,
    api_headers = c(
      Authorization = paste("Bearer", token),
      parse_custom_headers(Sys.getenv("ANTHROPIC_CUSTOM_HEADERS"))
    ),
    model = Sys.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-5-20250929"),
    system_prompt = paste(
      "You are a data cohort assistant for a Star Wars dataset with four related",
      "tables: people, planets, species and films. Use the provided tools to",
      "inspect available filters and to add/apply/toggle/clear filters on the",
      "user's behalf. Always call cb_get_filters_meta to discover exact filter",
      "ids and domains before applying values, and cb_describe_state to check",
      "current filters. Be concise."
    )
  )
}

# Named list of four related tibbles (people, planets, species, films).
starwars <- readRDS("inst/examples/starwars-assistant/starwars.rds")

starwars_binding_keys <- bind_keys(
  bind_key(update = data_key("people", "homeworld_id"), data_key("planets", "id")),
  bind_key(update = data_key("planets", "id"),          data_key("people", "homeworld_id")),
  bind_key(update = data_key("people", "species_id"),   data_key("species", "id")),
  bind_key(update = data_key("species", "id"),          data_key("people", "species_id")),
  bind_key(update = data_key("species", "homeworld_id"),data_key("planets", "id")),
  bind_key(update = data_key("planets", "id"),          data_key("species", "homeworld_id"))
)

# Descriptions are what the LLM reads via shape() / cb_get_filters_meta.
# `label` sets the short, human-readable filter name shown in the filter panel,
# while the description text carries the longer explanation.
starwars_description <- list(
  people = list(
    dataset_   = describe("Star Wars characters with physical attributes, species and home planet."),
    name       = describe("Character full name", label = "Name"),
    height     = describe("Character height in centimetres", label = "Height"),
    mass       = describe("Character body mass in kilograms", label = "Mass"),
    hair_color = describe("Character hair colour", label = "Hair colour"),
    skin_color = describe("Character skin colour", label = "Skin colour"),
    eye_color  = describe("Character eye colour", label = "Eye colour"),
    birth_year = describe("In-universe birth year, e.g. '19BBY'", label = "Birth year"),
    gender     = describe("Character gender (male, female, hermaphrodite, none)", label = "Gender")
  ),
  planets = list(
    dataset_        = describe("Planets appearing in the saga."),
    name            = describe("Planet name", label = "Name"),
    rotation_period = describe("Day length in hours", label = "Rotation period"),
    orbital_period  = describe("Year length in days", label = "Orbital period"),
    diameter        = describe("Diameter in kilometres", label = "Diameter"),
    gravity         = describe("Surface gravity relative to standard", label = "Gravity"),
    population      = describe("Number of sentient inhabitants", label = "Population"),
    climate         = describe("Prevailing climate(s)", label = "Climate"),
    terrain         = describe("Dominant terrain type(s)", label = "Terrain"),
    surface_water   = describe("Percentage of surface covered by water", label = "Surface water")
  ),
  species = list(
    dataset_         = describe("Sentient and non-sentient species."),
    name             = describe("Species name", label = "Name"),
    classification   = describe("Biological classification (mammal, reptile, artificial, ...)", label = "Classification"),
    designation      = describe("Designation, e.g. sentient", label = "Designation"),
    average_height   = describe("Average adult height in centimetres", label = "Average height"),
    average_lifespan = describe("Average lifespan in years", label = "Average lifespan"),
    language         = describe("Primary language", label = "Language")
  ),
  films = list(
    dataset_     = describe("The Star Wars feature films."),
    title        = describe("Film title", label = "Title"),
    episode_id   = describe("Episode number (1-6)", label = "Episode"),
    director     = describe("Film director", label = "Director"),
    producer     = describe("Film producer(s)", label = "Producer"),
    release_date = describe("Theatrical release date", label = "Release date")
  )
)

starwars_source <- set_source(
  tblist(
    people  = starwars$people,
    planets = starwars$planets,
    species = starwars$species,
    films   = starwars$films
  ),
  binding_keys = starwars_binding_keys,
  description  = starwars_description,
  compute_meta_stats = FALSE
) |>
  autofilter(attach_as = "meta")

# STARWARS_PREDEFINED_FILTERS toggles whether the app opens pre-filtered.
# Accepts truthy strings ("1", "true", "yes", "on"); defaults to TRUE.
use_predefined_filters <- local({
  raw <- tolower(trimws(Sys.getenv("STARWARS_PREDEFINED_FILTERS", "true")))
  if (!nzchar(raw)) TRUE else raw %in% c("1", "true", "yes", "on", "t")
})

# Pull a filter from the source's available_filters (created by autofilter(),
# so it carries the labels/descriptions defined above) and set its value.
# Reusing these avoids re-declaring filter ids/variables/names inline.
predefined_filter <- function(source, id, ...) {
  f <- purrr::detect(source$available_filters, ~ .x@id == id)
  if (is.null(f)) {
    stop(sprintf("No available filter with id '%s'.", id), call. = FALSE)
  }
  props <- list(...)
  for (nm in names(props)) {
    S7::prop(f, nm) <- props[[nm]]
  }
  f
}

# Predefined active filters (gender, height, species classification), chosen
# from the source's available_filters; run() applies them on startup.
starwars_cohort <- if (use_predefined_filters) {
  cohort(
    starwars_source,
    predefined_filter(starwars_source, "people-gender", value = "male"),
    predefined_filter(starwars_source, "people-height", range = c(150, 220)),
    predefined_filter(starwars_source, "species-classification", value = "mammal")
  ) |>
    run()
} else {
  cohort(starwars_source) |>
    run()
}

table_panel <- function(title, count_id, table_id) {
  bslib::nav_panel(
    title,
    shiny::h5(shiny::textOutput(count_id, inline = TRUE)),
    shiny::div(style = "overflow:auto; max-height:75vh;",
               shiny::tableOutput(table_id))
  )
}

# Used bare (no Shiny namespace) so cb_chat_server can read
# input[["<chat_id>_user_input"]].
chat_id <- "cohort_chat"

ui <- bslib::page_sidebar(
  title = "Star Wars \u2014 cohortBuilder + AI assistant",
  # assistant = FALSE: the chat lives in the right sidebar, not the filter panel.
  sidebar = bslib::sidebar(
    width = 420,
    cb_ui("starwars", assistant = FALSE, new_step = "configure")
  ),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      position = "right",
      width = 400,
      open = TRUE,
      title = "AI assistant",
      cb_chat_ui(chat_id)
    ),
    bslib::navset_card_tab(
      table_panel("people",  "n_people",  "tbl_people"),
      table_panel("planets", "n_planets", "tbl_planets"),
      table_panel("species", "n_species", "tbl_species"),
      table_panel("films",   "n_films",   "tbl_films")
    )
  )
)

server <- function(input, output, session) {
  chat <- build_chat()
  chat |> cb_register_tools(starwars_cohort)

  cb_server("starwars", starwars_cohort, stats = c("pre", "post"), feedback = FALSE)
  cb_chat_server(chat_id, chat, input, output, session)

  # The module emits "{id}-cb_data_updated"; ignoreNULL/ignoreInit = FALSE so
  # tables also render on first load (reflecting the predefined filters).
  post_data <- shiny::eventReactive(
    input[["starwars-cb_data_updated"]],
    {
      tryCatch(
        starwars_cohort$get_data(step_id = starwars_cohort$last_step_id(),
                                 state = "post"),
        error = function(e) starwars_cohort$get_source()$dtconn
      )
    },
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  render_for <- function(name) shiny::renderTable(utils::head(post_data()[[name]], 200))
  count_for  <- function(name) shiny::renderText(sprintf("%d rows", nrow(post_data()[[name]])))

  output$tbl_people  <- render_for("people")
  output$tbl_planets <- render_for("planets")
  output$tbl_species <- render_for("species")
  output$tbl_films   <- render_for("films")
  output$n_people  <- count_for("people")
  output$n_planets <- count_for("planets")
  output$n_species <- count_for("species")
  output$n_films   <- count_for("films")
}

shiny::runApp(
  shinyApp(ui, server),
  host = "0.0.0.0",
  port = as.integer(Sys.getenv("STARWARS_APP_PORT", "3838")),
  launch.browser = FALSE
)
