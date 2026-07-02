# shinyCohortBuilder 1.0.0

## New features

* Added domain-aware rendering via the `render_source` argument of `cb_server()`. With `render_source = "domain"` filter inputs are rendered from each filter's declared `domain` (validated eagerly at mount time), so the panel can render without scanning data. `render_source = "auto"` (default) keeps the previous statistics-based rendering.
* Added support for filter domains and domain propagation (including custom domains and the `propagate_domains` setting), and rendering refresh on domain changes.
* Added an LLM-based cohort assistant (`cb_chat_ui()` / `cb_chat_server()`) built on 'shinychat'.
* Added GUI support for the `datetime_range` filter.

## Improvements

* Feedback plots are now rendered client-side from minimal HTML structure (via `html_feedback_*()` helpers), with built-in rendering methods for each filter type.
* Rewrote `.gui_filter()` (now S7-based) for a clearer, less confusing filter API, and adapted to the new cohortBuilder filter property structure.
* Moved most GUI logic to cohortBuilder lifecycle hooks (filter add/remove, pending state, run step, update filter), simplifying action dispatch.
* Improved programmatic filter (de)activation.
* Modernized the filtering panel styling for a cleaner, more compact look: refreshed accordion step panels, toolbar and action buttons, filter input controls, and feedback plots, with consistent spacing and typography. Styling now derives from the active 'bslib' Bootstrap theme so panels adapt to custom themes (including dark mode), and disabled buttons are themed accordingly. Fixed several UI issues along the way (date-range input styling, Set/Get State button alignment, filter label/stat overflow).
* Dropped support for Bootstrap 3 (Bootstrap 4/5 only).
* Documented all exported and key internal functions.

## Bug fixes

* Fixed data statistics behavior, including showing real "previous step" statistics for the first step instead of a spurious "No data selected in previous step." placeholder.
* Fixed adding a step when filters propagate their values/domains.

# shinyCohortBuilder 0.4.0

* Fix running flow (and functionality of Run Button) when at least one non-active step is stale.
* Add an option to manage (add / remove) last step filters.
* Fix updating discrete filter through its feedback plot when post stats are turned off.
* Add gui support for `datatime_filter`.
* Update `ggplot2` version and adjust parameter name.

# shinyCohortBuilder 0.3.1

* Fix compatibility with latest version of glue package (#61)

# shinyCohortBuilder 0.3.0

* Most of the modals have now `easyClosed` option turned on (#25).
* Fix positioning of filter help modal buttons (#37).
* Fix rendering feedback plots when special character are included in data (#58).
* `demo_app` can now return `shiny::shinyApp` object (#56)
* Added new filter of type `query` based on 'shinyQueryBuilder' widget.
* Added customization for filtering panel button classes via `scb_button_type` option.
* Added customization for filtering panel labels via `scb_labels` option.
* Added customization for filtering panel icons via `scb_icons` option.
* Added customization for filter feedback plots color palette via `scb_chart_palette` option.
* Fixed `discrete_filter` modal visibility with backdrop turned on.
* Modified look for filter activating switch and filter containers to improve visibility.
* Fixed rendering all the filter values for discrete filter when `gui_input="vs"` is used (#47).

# shinyCohortBuilder 0.2.1

* Fixed error showing up when user deactivates filter with more than one value set.
* Added `gui_args` argument that can be set to `filter`. 
  The argument stores list of arguments passed to input controllers.

# shinyCohortBuilder 0.2.0 

* Arguments `feedback` and `stats` passed to filter definitions are now respected and override the arguments passed to `cb_server`.
* Fix updating filters data when other filters are changed (https://github.com/r-world-devs/shinyCohortBuilder/issues/27).
* Implement bugs related to `multi_discrete` filter (https://github.com/r-world-devs/shinyCohortBuilder/issues/9).
* `onUnload` hook implemented to clear custom package hooks (https://github.com/r-world-devs/shinyCohortBuilder/issues/21).

# shinyCohortBuilder 0.1

* First release.
