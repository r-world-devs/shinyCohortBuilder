# Tests for domain-aware filter rendering (render_source / domain mode).

# Build a cohort whose filters declare a domain. `cache` controls eager vs lazy
# statistics; `stats` / `feedback` are set on the cohort attributes to emulate
# what cb_server() would do.
build_domain_cohort <- function(cache = TRUE, stats = c("pre", "post"),
                                feedback = TRUE, render_source = "auto") {
  source <- cohortBuilder::set_source(
    cohortBuilder::tblist(iris = iris)
  )
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris", variable = "Species",
      domain = c("setosa", "versicolor", "virginica")
    ),
    cohortBuilder::filter(
      "range", id = "sl", dataset = "iris", variable = "Sepal.Length",
      domain = c(4, 8)
    ),
    cache = cache
  )
  coh$attributes$stats <- stats
  coh$attributes$feedback <- feedback
  coh$attributes$render_source <- render_source
  coh
}

# -- resolve_render_mode() ----------------------------------------------------

test_that("resolve_render_mode returns 'domain' only when stats NULL and feedback FALSE", {
  coh <- build_domain_cohort(stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "species")
  expect_identical(resolve_render_mode(filter, coh)$mode, "domain")
})

test_that("resolve_render_mode returns 'stats' when stats requested", {
  coh <- build_domain_cohort(stats = c("pre", "post"), feedback = FALSE)
  filter <- coh$get_filter("1", "species")
  expect_identical(resolve_render_mode(filter, coh)$mode, "stats")
})

test_that("resolve_render_mode returns 'stats' when only feedback is on", {
  coh <- build_domain_cohort(stats = NULL, feedback = TRUE)
  filter <- coh$get_filter("1", "species")
  expect_identical(resolve_render_mode(filter, coh)$mode, "stats")
})

test_that("resolve_render_mode honors per-filter feedback override", {
  # Cohort is in stats mode purely via feedback (stats already NULL). A filter
  # that disables feedback then resolves to domain mode.
  coh <- build_domain_cohort(stats = NULL, feedback = TRUE)
  filter <- coh$get_filter("1", "species")
  expect_identical(resolve_render_mode(filter, coh)$mode, "stats")

  filter@extra$feedback <- FALSE
  expect_identical(resolve_render_mode(filter, coh)$mode, "domain")
})

test_that("resolve_render_mode exposes render_source", {
  coh <- build_domain_cohort(render_source = "domain")
  filter <- coh$get_filter("1", "species")
  expect_identical(resolve_render_mode(filter, coh)$render_source, "domain")
})

# -- discrete domain rendering ------------------------------------------------

test_that("discrete_input_params returns domain choices in domain mode", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "species")

  params <- discrete_input_params(filter, "1-species", coh)
  values <- params$choiceValues %||% names(params$choices)
  expect_setequal(values, c("setosa", "versicolor", "virginica"))
})

test_that("discrete_input_params does not populate cache in domain mode", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "species")

  discrete_input_params(filter, "1-species", coh)

  # No cache entry should have been computed for the filter.
  cached <- coh$get_cache("1", "species", state = "pre", .recalc_when_missing = FALSE)
  expect_null(cached)
})

# -- range domain rendering ---------------------------------------------------

test_that("range_input_params returns domain min/max in domain mode", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "sl")

  params <- range_input_params(filter, "1-sl", coh)
  expect_identical(params$min, 4)
  expect_identical(params$max, 8)
})

test_that("range_input_params does not populate cache in domain mode", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "sl")

  range_input_params(filter, "1-sl", coh)

  cached <- coh$get_cache("1", "sl", state = "pre", .recalc_when_missing = FALSE)
  expect_null(cached)
})

# -- cache access is fully avoided in domain mode (spy) -----------------------

# Replace the cohort's R6 cache methods with spies that record invocations.
# Returns a list with the patched cohort and counters; restore on exit.
spy_cache <- function(coh) {
  counts <- new.env(parent = emptyenv())
  counts$get <- 0L
  counts$update <- 0L

  orig_get <- coh$get_cache
  orig_update <- coh$update_cache

  unlockBinding("get_cache", coh)
  unlockBinding("update_cache", coh)
  coh$get_cache <- function(...) {
    counts$get <- counts$get + 1L
    orig_get(...)
  }
  coh$update_cache <- function(...) {
    counts$update <- counts$update + 1L
    orig_update(...)
  }

  list(
    counts = counts,
    restore = function() {
      coh$get_cache <- orig_get
      coh$update_cache <- orig_update
    }
  )
}

test_that("discrete domain rendering never touches the cache (spy)", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "species")

  spy <- spy_cache(coh)
  on.exit(spy$restore(), add = TRUE)

  discrete_input_params(filter, "1-species", coh)

  expect_identical(spy$counts$get, 0L)
  expect_identical(spy$counts$update, 0L)
})

test_that("range domain rendering never touches the cache (spy)", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  filter <- coh$get_filter("1", "sl")

  spy <- spy_cache(coh)
  on.exit(spy$restore(), add = TRUE)

  range_input_params(filter, "1-sl", coh)

  expect_identical(spy$counts$get, 0L)
  expect_identical(spy$counts$update, 0L)
})

test_that("stats mode does read the cache (spy sanity check)", {
  coh <- build_domain_cohort(
    cache = TRUE, stats = c("pre", "post"), feedback = TRUE
  ) |> cohortBuilder::run()
  filter <- coh$get_filter("1", "species")

  spy <- spy_cache(coh)
  on.exit(spy$restore(), add = TRUE)

  discrete_input_params(filter, "1-species", coh)

  # Stats mode must consult the cache at least once - guards against the spy
  # silently passing because nothing reads the cache.
  expect_gt(spy$counts$get, 0L)
})

# -- render_source = "domain" in stats mode -----------------------------------

test_that("render_source = 'domain' uses domain bounds even with stats", {
  coh <- build_domain_cohort(
    cache = TRUE, stats = c("pre", "post"), feedback = TRUE,
    render_source = "domain"
  ) |> cohortBuilder::run()
  coh$attributes$stats <- c("pre", "post")
  coh$attributes$feedback <- TRUE
  coh$attributes$render_source <- "domain"

  filter <- coh$get_filter("1", "sl")
  params <- range_input_params(filter, "1-sl", coh)
  # Domain is c(4, 8); the empirical range of iris Sepal.Length is c(4.3, 7.9).
  expect_identical(params$min, 4)
  expect_identical(params$max, 8)
})

test_that("render_source = 'domain' shows full vocabulary for discrete", {
  coh <- build_domain_cohort(
    cache = TRUE, stats = c("pre", "post"), feedback = TRUE,
    render_source = "domain"
  ) |> cohortBuilder::run()
  coh$attributes$stats <- c("pre", "post")
  coh$attributes$feedback <- TRUE
  coh$attributes$render_source <- "domain"

  filter <- coh$get_filter("1", "species")
  params <- discrete_input_params(filter, "1-species", coh)
  values <- params$choiceValues %||% names(params$choices)
  expect_setequal(values, c("setosa", "versicolor", "virginica"))
})

test_that("render_source = 'domain' overlays pre and post counts, 0 for absent", {
  for (as_char in c(FALSE, TRUE)) {
    dat <- iris
    if (as_char) dat$Species <- as.character(dat$Species)
    source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = dat))
    coh <- cohortBuilder::cohort(
      source,
      cohortBuilder::filter(
        "discrete", id = "species", dataset = "iris", variable = "Species",
        domain = c("setosa", "versicolor", "virginica")
      ),
      cache = TRUE
    )
    coh$attributes$stats <- c("pre", "post")
    coh$attributes$feedback <- TRUE
    coh$attributes$render_source <- "domain"
    coh <- cohortBuilder::update_filter(
      coh, 1, "species", value = c("setosa", "versicolor")
    ) |> cohortBuilder::run()

    filter <- coh$get_filter("1", "species")
    params <- discrete_input_params(filter, "1-species", coh)
    labels <- vapply(params$choiceNames, as.character, character(1))

    # Both pre and post stats are shown (post / pre).
    expect_true(all(grepl(" / ", labels)), info = paste("as_char =", as_char))
    # The filtered-out value reads 0, never the literal "NULL".
    virginica <- labels[grepl("virginica", labels)]
    expect_match(virginica, ">0<")
    expect_false(grepl("NULL", virginica), info = paste("as_char =", as_char))
  }
})

# -- edge case: domain mode but no domain -------------------------------------

test_that("domain mode with NULL domain warns and renders nothing", {
  source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = iris))
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "nd", dataset = "iris", variable = "Species"
    ),
    cache = FALSE
  )
  coh$attributes$stats <- NULL
  coh$attributes$feedback <- FALSE
  coh$attributes$render_source <- "auto"

  filter <- coh$get_filter("1", "nd")
  expect_warning(
    params <- discrete_input_params(filter, "1-nd", coh)
  )
  expect_length(params$choices, 0)
})

# -- data stats follow the stats setting (R5b) --------------------------------

test_that(".update_data_stats is a no-op when stats is NULL", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)

  session <- list(ns = function(x) x)
  res <- .update_data_stats(coh$get_source(), "1", coh, session)
  expect_null(res)

  # No cache entry should have been computed by the no-op.
  cached <- coh$get_cache("1", state = "post", .recalc_when_missing = FALSE)
  expect_null(cached)
})

test_that(".update_data_stats.default shows placeholder when parent cache is absent", {
  # Regression: the default method used to read a flat `$n_rows` (which never
  # exists, the step cache nests per-dataset) and then evaluate `if (!previous >
  # 0)`. When the parent step was never run the cache is NULL, so `previous` was
  # NULL and the comparison errored with "argument is of length zero", crashing
  # the app (run_button mode / cache = FALSE / freshly added step). The method
  # must instead read without forcing a recompute and fall back to the
  # placeholder.
  coh <- build_domain_cohort(cache = FALSE, stats = c("pre", "post"), feedback = FALSE)
  # Not run: step "1" pre cache is absent.
  expect_null(coh$get_cache("1", state = "pre", .recalc_when_missing = FALSE))

  session <- list(ns = function(x) x)
  captured <- NULL
  expect_no_error(
    testthat::with_mocked_bindings(
      .update_data_stats.default(coh$get_source(), "1", coh, session),
      removeUI = function(...) invisible(NULL),
      insertUI = function(selector, ui, ...) {
        captured <<- ui
        invisible(NULL)
      },
      .package = "shiny"
    )
  )
  expect_identical(captured, "No data selected in previous step.")
})

# -- state round-trip preserves domain (R5c) ----------------------------------

test_that("get_state/restore preserves filter domains", {
  coh <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  state <- coh$get_state(json = FALSE)

  coh2 <- build_domain_cohort(cache = FALSE, stats = NULL, feedback = FALSE)
  coh2$restore(state)

  expect_identical(
    cohortBuilder::filter_domain(coh2$get_filter("1", "species")),
    c("setosa", "versicolor", "virginica")
  )
  expect_identical(
    cohortBuilder::filter_domain(coh2$get_filter("1", "sl")),
    c(4, 8)
  )

  # The restored cohort still renders in domain mode without scanning the source.
  filter <- coh2$get_filter("1", "species")
  params <- discrete_input_params(filter, "1-species", coh2)
  values <- params$choiceValues %||% names(params$choices)
  expect_setequal(values, c("setosa", "versicolor", "virginica"))
  cached <- coh2$get_cache("1", "species", state = "pre", .recalc_when_missing = FALSE)
  expect_null(cached)
})

# -- render_source = "domain" domain-presence validation (req 1) ---------------

test_that("validate_domains_present passes when every filter has a domain", {
  coh <- build_domain_cohort()
  expect_true(validate_domains_present(coh))
})

test_that("validate_domains_present errors listing filters without a domain", {
  source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = iris))
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris", variable = "Species",
      domain = c("setosa", "versicolor", "virginica")
    ),
    # No domain declared.
    cohortBuilder::filter("range", id = "sl", dataset = "iris", variable = "Sepal.Length")
  )
  expect_error(
    validate_domains_present(coh),
    "requires every filter to declare a domain"
  )
  expect_error(validate_domains_present(coh), "filter 'sl'")
})

test_that("render_source = 'domain' is allowed with propagate_domains = 'none' when domains are set", {
  source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = iris))
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter(
      "discrete", id = "species", dataset = "iris", variable = "Species",
      domain = c("setosa", "versicolor", "virginica")
    ),
    cohortBuilder::filter(
      "range", id = "sl", dataset = "iris", variable = "Sepal.Length",
      domain = c(4, 8)
    ),
    propagate_domains = "none"
  )
  # Domains are user-declared, so domain rendering is honoured even without
  # propagation: validation must not reject the "none" mode itself.
  expect_true(validate_domains_present(coh))
})

test_that("render_source = 'domain' errors when a filter lacks a domain regardless of propagation", {
  source <- cohortBuilder::set_source(cohortBuilder::tblist(iris = iris))
  coh <- cohortBuilder::cohort(
    source,
    cohortBuilder::filter("range", id = "sl", dataset = "iris", variable = "Sepal.Length"),
    propagate_domains = "filter"
  )
  expect_error(
    validate_domains_present(coh),
    "requires every filter to declare a domain"
  )
})
