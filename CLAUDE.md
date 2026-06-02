# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

shinyCohortBuilder is the Shiny GUI layer for the [cohortBuilder](https://r-world-devs.github.io/cohortBuilder/) R package. It renders interactive filtering panels in Shiny apps — users build cohorts by combining filters across datasets and steps. The core filtering logic lives in cohortBuilder; this package provides the UI, feedback plots, and action dispatch system.

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

The package exposes a single Shiny module pair. `cb_ui(id)` renders the panel structure (accordion steps, toolbar buttons). `cb_server(id, cohort)` takes a cohortBuilder `cohort` R6 object and wires up all reactivity. The cohort object is created externally and passed in — the module doesn't own it.

### Filter Type System (R/filter_*.R)

Each filter type implements `.gui_filter.<type>()` — an S3 method returning a list with `input()`, `feedback()`, `server()`, and `update()` functions. Filter types: `discrete`, `range`, `date_range`, `datetime_range`, `discrete_text`, `multi_discrete`, `query`. To add a new filter type, create a new `R/filter_<type>.R` file implementing the `.gui_filter` S3 method.

### Action Dispatch (R/actions.R)

User interactions flow through a single action dispatcher. JS sets `input$action = {id: "action_name", params: {...}}`, and the server routes to `gui_*` handler functions. Key actions: `update_filter`, `add_step`, `rm_step`, `clear_step`, `run_step`, `show_repro_code`, `restore_state`, `show_attrition`. `.trigger_action_js()` generates the JS onclick code; `.trigger_action()` triggers from R server-side.

### Hooks (R/cb_layer.R)

The package integrates with cohortBuilder's lifecycle via hooks registered in `.onLoad()`. Hooks like `post_run_step_hook`, `post_update_filter_hook`, `post_add_step_hook` bridge R6 events to Shiny UI updates via `session$sendCustomMessage()`. The JS side (`srcjs/scb.js`) has matching `Shiny.addCustomMessageHandler()` listeners.

### Source Layer Extensibility

Source types (default: `tblist`) define S3 methods for rendering: `.render_filters`, `.update_data_stats`, `.step_attrition`, `.available_filters_choices`, `.filter_position`, `autofilter`. The `tblist` implementations in `R/source_tblist.R` group filters by dataset and show per-dataset statistics.

### R ↔ JS Communication

- **R → JS**: `session$sendCustomMessage("handler_name", data)` — handlers defined in `srcjs/scb.js`
- **JS → R**: `Shiny.setInputValue()` via `.cb_input` containers — JS monitors `.cb_input` elements and only forwards user-initiated changes (ignores programmatic updates via `update*` functions)
- The `.cb_input()` wrapper (in `R/renders.R`) marks inputs so JS can distinguish user changes from server updates

### Feedback Plots

Interactive plots use `ggplot2` + `ggiraph`. Each filter type's `feedback()` returns a `renderGirafe` function. Plots show pre/post filtering distributions and respond to filter changes.

## Key Files

- `R/renders.R` — Main rendering pipeline: `cb_ui`, `cb_server`, `.render_filter`, `.render_filters`, step rendering, data stats
- `R/actions.R` — All action handlers (`gui_*` functions) and the action dispatch observer
- `R/cb_layer.R` — Hook implementations connecting cohortBuilder lifecycle to Shiny, `.onLoad`/`.onUnload`
- `R/app.R` — `demo_app()` and `gui()` convenience functions
- `R/chat.R` — LLM assistant integration (`cb_chat_ui`/`cb_chat_server`)
- `R/ui_utils.R` — `button()`, `panel()`, `scb_labels`/`scb_icons` defaults, `cb_changed()`
- `srcjs/scb.js` — All client-side JS: custom message handlers, input change detection, idle state tracking

## Customization System

The package uses global R options for UI customization:
- `scb_labels` — button/panel text labels
- `scb_icons` — `shiny::icon()` specifications
- `scb_chart_palette` — feedback plot colors
- `scb_minified` — toggle minified vs debug JS/CSS
- `scb_verbose` — enable console debug logging

## Branching

- `master` — main/release branch
- `dev` — development integration branch
- `krystian.ai` — feature branch for LLM assistant integration
