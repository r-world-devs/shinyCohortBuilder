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
