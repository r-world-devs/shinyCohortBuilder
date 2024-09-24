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
