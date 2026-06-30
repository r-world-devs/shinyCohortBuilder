# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

shinyCohortBuilder is the Shiny GUI layer for the [cohortBuilder](https://r-world-devs.github.io/cohortBuilder/) R package. It renders interactive filtering panels in Shiny apps — users build cohorts by combining filters across datasets and steps. The core filtering logic lives in cohortBuilder; this package provides the UI, feedback visualizations, and action dispatch system.

## Development Commands

```bash
# R package development (run in R console)
devtools::load_all()           # Load package for interactive dev
devtools::test()               # Run testthat tests
devtools::test_active_file()   # Run tests in the current file
devtools::document()           # Regenerate NAMESPACE and man/ from roxygen2
devtools::check()              # Full R CMD check

# JavaScript/CSS assets (run in shell)
npm run development            # Webpack dev build (srcjs/ → inst/www/scb.min.js)
npm run production             # Webpack production build (minified)
npm run watch                  # Watch mode for JS development
npm run none                   # Unminified build for debugging

# Run the demo app (in R console)
shinyCohortBuilder::demo_app()
```

JS source is in `srcjs/scb.js`, SCSS in `srcscss/scb.scss`. Built assets go to `inst/www/`. The `scb.js` and `scb.css` (unminified) copies in `inst/www/` are also committed for `options("scb_minified" = FALSE)` debugging.

## Architecture

### Core Module: `cb_ui()` / `cb_server()`

The package exposes a single Shiny module pair. `cb_ui(id)` renders the panel structure (accordion steps, toolbar buttons). `cb_server(id, cohort, ...)` takes a cohortBuilder `cohort` R6 object and wires up all reactivity. The cohort object is created externally and passed in — the module doesn't own it. Key `cb_server()` args: `run_button` (`"none"`/`"local"`/`"global"` deferred-run), `stats` (which data-stats to show, e.g. `c("pre", "post")`), `feedback` (show feedback plots), and `render_source` (`"auto"` vs `"domain"`, see Domain-aware Rendering below). Runtime config is stored on `cohort$attributes` via `restore_attribute()` so hooks and renderers can read it back.

### Domain-aware Rendering (`render_source`)

`render_source` controls where filter inputs derive their available choices/ranges from:
- `"auto"` (default) — render from cached step statistics (requires `compute_stats`).
- `"domain"` — render each input from its filter's declared `domain` (a cohortBuilder filter property), so the UI can render without scanning data.

`render_source = "domain"` requires *every* filter in every step to declare a domain. `cb_server()` validates this eagerly at mount time via `validate_domains_present()` (R/control_utils.R), erroring with the list of filters missing a domain rather than failing later per-render. Any `propagate_domains` mode (including `"none"`) is allowed as long as domains are present — propagation is a cohortBuilder concern; this layer only needs a domain to render from. Filters get domains either explicitly (`filter("discrete", ..., domain = c(...))`) or via propagation / step copying. See `inst/examples/domains/app.R` for an interactive configurator covering `stats` / `feedback` / `propagate_domains` / `render_source` / `run_button` combinations.

### Filter Type System (R/filter_*.R)

Each filter type implements `.gui_filter.<type>()` — an S3 method returning a list with:
- `input()` — renders filter input UI
- `feedback()` — returns list with `plot_id`, `output_fun`, and `render_fun` for HTML-based feedback
- `server()` — sets up reactive observers for the filter
- `update()` — updates filter UI when value changes server-side
- `post_stats` — boolean indicating if post-stats should be shown
- `multi_input` — boolean indicating multiple input types available

Filter types: `discrete`, `range`, `date_range`, `datetime_range`, `discrete_text`, `multi_discrete`, `query`. To add a new filter type, create a new `R/filter_<type>.R` file implementing the `.gui_filter` S3 method.

### Feedback Visualizations (R/feedback_html.R + srcjs/scb.js)

Feedback is rendered client-side in JavaScript. R generates minimal HTML structure via `html_feedback_*()` helper functions, embedding filter data as JSON in `data-feedback` attributes. JS renders the visual bars/histograms on the client.

Helper functions in `R/feedback_html.R`:
- `html_feedback_bar()` — discrete filter bars with color-coded segments
- `html_feedback_hist()` — histograms for range/date filters
- `html_feedback_text_bar()` — discrete text filter bars (selected vs not selected)
- `html_feedback_multi_bar()` — grouped bars for multi-discrete filters

JS side (`srcjs/scb.js`) handles `update_feedback` and `set_feedback_palette` custom messages, plus auto-renders feedback from `data-feedback` attributes via a MutationObserver.

### Action Dispatch (R/actions.R)

User interactions flow through a single action dispatcher. JS sets `input$action = {id: "action_name", params: {...}}`, and the server routes to `gui_*` handler functions. Key actions: `update_filter`, `add_step`, `rm_step`, `clear_step`, `run_step`, `show_repro_code`, `restore_state`, `show_attrition`. `.trigger_action_js()` generates the JS onclick code; `.trigger_action()` triggers from R server-side.

### Hooks (R/cb_layer.R)

The package integrates with cohortBuilder's lifecycle via hooks registered in `.onLoad()`. The hook system is the primary mechanism for bridging R6 events to Shiny UI updates — most GUI logic (filter add/remove, pending state, run step, data stats) is driven by hooks rather than explicit action handlers.

Key hooks:
- `post_run_step_hook` — updates UI after step runs (data stats, feedback)
- `post_update_filter_hook` — updates feedback when filter value changes
- `post_add_step_hook` — renders new step UI
- `post_set_pending_hook` — triggers GUI pending state updates
- `post_add_filter_hook` — handles GUI filter addition
- `post_rm_filter_hook` — handles GUI filter removal
- `post_init_source_hook` — attaches GUI to all filters after source initialization

### Source Layer Extensibility

Source types (default: `tblist`) define S3 methods for rendering: `.render_filters`, `.update_data_stats`, `.step_attrition`, `.available_filters_choices`, `.filter_position`, `autofilter`. The `tblist` implementations in `R/source_tblist.R` group filters by dataset and show per-dataset statistics. `.filter_position()` (defined in `R/source.R`) allows source types to control where filters are positioned in the UI.

### R ↔ JS Communication

- **R → JS**: `session$sendCustomMessage("handler_name", data)` — handlers defined in `srcjs/scb.js`
- **JS → R**: `Shiny.setInputValue()` via `.cb_input` containers — JS monitors `.cb_input` elements and only forwards user-initiated changes (ignores programmatic updates via `update*` functions)
- The `.cb_input()` wrapper (in `R/renders.R`) marks inputs so JS can distinguish user changes from server updates

## Key Files

- `R/renders.R` — Main rendering pipeline: `cb_ui`, `cb_server`, `.render_filter`, `.render_filters`, step rendering, data stats
- `R/actions.R` — All action handlers (`gui_*` functions) and the action dispatch observer
- `R/cb_layer.R` — Hook implementations connecting cohortBuilder lifecycle to Shiny, `.onLoad`/`.onUnload`
- `R/feedback_html.R` — HTML feedback visualization helpers (`html_feedback_bar`, `html_feedback_hist`, etc.)
- `R/source.R` — Base S3 method for `.filter_position()`
- `R/control_utils.R` — Small utility helpers (`if_null_default`, `modify_list`, `suff`, etc.) and `validate_domains_present()` for `render_source = "domain"`
- `R/app.R` — `demo_app()` and `gui()` convenience functions
- `R/chat.R` — LLM assistant integration (`cb_chat_ui`/`cb_chat_server`) via `shinychat`
- `R/ui_utils.R` — `button()`, `panel()`, `scb_labels`/`scb_icons` defaults, `cb_changed()`
- `srcjs/scb.js` — All client-side JS: custom message handlers, feedback rendering, input change detection, idle state tracking

## Testing

Tests use `testthat` (unit tests) and `shinytest2` (UI integration tests).

- `tests/testthat/test-*.R` — Unit tests for filters, rendering, actions, `.cb_input` behavior
- `tests/testthat/apps/` — shinytest2 test apps for different configurations (basic filtering, feedback, run button, stats display, step configuration)

## Customization System

The package uses global R options for UI customization:
- `scb_labels` — button/panel text labels
- `scb_icons` — `shiny::icon()` specifications
- `scb_chart_palette` — feedback plot colors (configurable via `set_feedback_palette` JS handler)
- `scb_minified` — toggle minified vs debug JS/CSS
- `scb_verbose` — enable console debug logging

## Branching

- `master` — main/release branch
- `dev` — development integration branch
- `feature/domains` — current development branch (domain-aware rendering, `render_source`, data-stats fixes)
