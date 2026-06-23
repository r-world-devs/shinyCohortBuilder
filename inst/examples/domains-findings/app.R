# Domain-propagation findings playground
# =======================================
#
# A self-contained app to manually reproduce the non-obvious domain-propagation
# behaviours of cohortBuilder + shinyCohortBuilder.
#
# Layout:
#   * Left  : the cohortBuilder filtering panel (the thing under test).
#   * Right : a CONFIG box (choose propagate_domains / render_source / run_button
#             / cache, then click "Apply" to rebuild the panel) and a live
#             INSPECTOR that polls the cohort object twice a second and prints,
#             for every step:
#               - is_pending  (TRUE while a step is awaiting a run)
#               - group domain (the discrete filter's current declared domain)
#               - age domain   (the range filter's current declared domain)
#             plus, for the age slider, the LIVE widget bounds read from the
#             browser (ionRangeSlider) so you can compare them to the static HTML
#             attributes (Finding 3).
#
# The inspector is the key tool: it lets you watch domains narrow (or not) as you
# act, without opening the R console.
#
# Run with:
#   shiny::runApp(
#     system.file("examples/domains-findings", package = "shinyCohortBuilder")
#   )
# or, from a source checkout:
#   pkgload::load_all("cohortBuilder"); pkgload::load_all("shinyCohortBuilder")
#   shiny::runApp("shinyCohortBuilder/inst/examples/domains-findings")

library(shiny)
library(cohortBuilder)
library(shinyCohortBuilder)

# --- Data with a clear narrowing story --------------------------------------
# group "A" occurs ONLY with gender "M". So restricting gender to "F" removes
# every "A" row: the remaining groups are {B, C} and ages span 35-50.
patients <- data.frame(
  id     = 1:12,
  group  = factor(c("B", "A", "C", "B", "C", "A", "A", "B", "C", "B", "A", "C")),
  gender = factor(c("F", "M", "F", "F", "F", "M", "M", "F", "F", "M", "M", "F")),
  age    = c(50L, 28L, 38L, 49L, 45L, 33L, 43L, 35L, 40L, 61L, NA, 47L)
)

# Build a cohort. Gender starts at "F"; in the walkthrough you first WIDEN it to
# {F, M} (so step 2 sees the full domain), then NARROW it back to {F} to trigger
# downstream propagation. Starting from a concrete value (rather than NULL) keeps
# the discrete filter's update semantics straightforward.
build_cohort <- function(cache, propagate_domains) {
  cohort(
    set_source(tblist(patients = patients)),
    filter(
      "discrete", id = "gender", name = "Gender", dataset = "patients",
      variable = "gender", value = "F", domain = c("F", "M")
    ),
    filter(
      "range", id = "age", name = "Age", dataset = "patients",
      variable = "age", range = NA, domain = c(18, 80)
    ),
    filter(
      "discrete", id = "group", name = "Group", dataset = "patients",
      variable = "group", value = NA, domain = c("A", "B", "C")
    ),
    cache = cache,
    propagate_domains = propagate_domains
  )
}

# JS: every 0.5s, read step-2 age slider's live bounds + its static HTML
# data-min/data-max attributes, and push both back to R as a Shiny input. This
# is what makes Finding 3 (live widget bounds change but HTML attrs do not)
# visible directly in the app.
slider_probe_js <- "
$(function(){
  setInterval(function(){
    var sel = '[id$=\"-2-age-slider\"]';
    var el = $(sel).first();
    if (el.length === 0) {
      Shiny.setInputValue('age_slider_probe', {present:false}, {priority:'event'});
      return;
    }
    var irs = el.data('ionRangeSlider');
    var live = irs ? {min: irs.options.min, max: irs.options.max,
                      from: irs.result.from, to: irs.result.to} : null;
    var attr = {min: el.attr('data-min'), max: el.attr('data-max')};
    Shiny.setInputValue('age_slider_probe',
      {present:true, live:live, attr:attr}, {priority:'event'});
  }, 500);
});
"

ui <- bslib::page_sidebar(
  title = "Domain-propagation findings playground",
  tags$head(tags$script(HTML(slider_probe_js))),
  sidebar = bslib::sidebar(
    title = "Config & inspector", position = "right", width = 380, open = TRUE,
    selectInput("cfg_propagate", "propagate_domains",
                c("none", "filter", "cache", "data"), selected = "data"),
    selectInput("cfg_render", "render_source",
                c("auto", "domain"), selected = "domain"),
    selectInput("cfg_run_button", "run_button",
                c("none", "local", "global"), selected = "none"),
    checkboxInput("cfg_cache", "cache", value = TRUE),
    actionButton("cfg_apply", "Apply (rebuild panel)",
                 class = "btn-primary", width = "100%"),
    tags$hr(),
    tags$strong("Live inspector (polls cohort every 0.5s)"),
    verbatimTextOutput("inspector"),
    tags$strong("Age slider — LIVE widget bounds (step 2)"),
    verbatimTextOutput("slider_live")
  ),
  bslib::card(
    bslib::card_header("Filtering panel"),
    div(id = "panel_container",
        div(class = "text-muted", "Pick a config on the right and click Apply."))
  )
)

server <- function(input, output, session) {

  state <- reactiveValues(id = NULL, coh = NULL)
  counter <- reactiveVal(0L)

  observeEvent(input$cfg_apply, {
    n <- counter() + 1L
    counter(n)
    id <- paste0("coh", n)
    coh <- build_cohort(
      cache = isTRUE(input$cfg_cache),
      propagate_domains = input$cfg_propagate
    )
    if (identical(input$cfg_render, "domain")) {
      ok <- tryCatch({
        shinyCohortBuilder:::validate_domains_present(coh); TRUE
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        FALSE
      })
      if (!ok) return(invisible(NULL))
    }
    state$id <- id
    state$coh <- coh

    # Insert the panel UI into the DOM FIRST, then mount the server. cb_server's
    # render_steps() targets the panel container, so the container must already
    # exist when the server runs (this is why cb_ui inside renderUI does not work
    # here). We clear any previous panel and insert the fresh one synchronously.
    removeUI(selector = "#panel_container > *", multiple = TRUE, immediate = TRUE)
    insertUI(
      selector = "#panel_container", where = "beforeEnd", immediate = TRUE,
      ui = cb_ui(id, steps = TRUE, state = FALSE, code = FALSE, attrition = TRUE)
    )
    cb_server(
      id, coh,
      run_button = input$cfg_run_button,
      stats = c("pre", "post"),
      feedback = FALSE,
      render_source = input$cfg_render,
      enable_bookmarking = "disable",
      show_help = FALSE
    )
  })

  # Poll the cohort object so the inspector reflects domains/pending live.
  output$inspector <- renderPrint({
    invalidateLater(500, session)
    coh <- state$coh
    if (is.null(coh)) {
      cat("(no cohort yet)\n"); return(invisible())
    }
    cat("propagate_domains =", coh$get_propagate_domains_mode(), "\n")
    cat("steps =", paste(names(coh$get_step()), collapse = ", "), "\n\n")
    for (sid in names(coh$get_step())) {
      st <- coh$get_step(sid)
      gd <- tryCatch(filter_domain(st$filters[["group"]]), error = function(e) NA)
      ad <- tryCatch(filter_domain(st$filters[["age"]]),   error = function(e) NA)
      pend <- tryCatch(coh$is_pending(sid), error = function(e) NA)
      cat(sprintf(
        "step %s | pending=%s | group domain={%s} | age domain={%s}\n",
        sid, pend,
        paste(gd, collapse = ","), paste(ad, collapse = ",")
      ))
    }
  })

  # Show the step-2 age slider's LIVE widget bounds next to its STATIC HTML
  # data-min/data-max attributes. Finding 3: after propagation the live bounds
  # change but the HTML attributes stay at the originally-rendered values.
  output$slider_live <- renderPrint({
    p <- input$age_slider_probe
    if (is.null(p) || isFALSE(p$present)) {
      cat("(add a 2nd step to see the step-2 age slider)\n"); return(invisible())
    }
    if (is.null(p$live)) { cat("(slider not initialised yet)\n"); return(invisible()) }
    cat(sprintf("LIVE widget bounds : min=%s max=%s (handles %s..%s)\n",
                p$live$min, p$live$max, p$live$from, p$live$to))
    cat(sprintf("STATIC HTML attrs  : data-min=%s data-max=%s\n",
                p$attr$min, p$attr$max))
    if (!is.null(p$live$min) && !identical(as.character(p$live$min), p$attr$min)) {
      cat(">>> LIVE and STATIC differ -> Finding 3 reproduced.\n")
    }
  })
}

shinyApp(ui, server)
