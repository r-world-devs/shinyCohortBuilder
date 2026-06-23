if_null_default <- function(val, default) {
  if (is.null(val)) {
    return(default)
  }
  return(val)
}

if_na_default <- function(val, default) {
  ifelse(is.na(val), default, val)
}

is_none <- function(x) {
  identical(x, "none")
}

modify_list <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  return(
    utils::modifyList(x, y, keep.null = TRUE)
  )
}

suff <- function(x, suffix) {
  paste0(x, "-", suffix)
}

# Resolve, for a single filter, whether it renders from cached statistics
# ("stats" mode) or from its declared domain ("domain" mode), plus the
# `render_source` strategy used in stats mode.
#
# A filter is in domain mode iff its resolved `stats` is NULL AND its resolved
# `feedback` is FALSE (per-filter `filter@extra` overrides take precedence over
# the cohort-level `cb_server()` defaults). Otherwise it is in stats mode.
#
# Returns a list with:
# - mode: "domain" or "stats"
# - render_source: "auto" or "domain" (only meaningful in stats mode; controls
#   whether choice/range bounds come from stats or from the full domain)
# - stats / feedback: the resolved per-filter values (for downstream reuse)
resolve_render_mode <- function(filter, cohort) {
  stats <- if_null_default(filter@extra$stats, cohort$attributes$stats)
  feedback <- if_null_default(filter@extra$feedback, cohort$attributes$feedback)
  feedback <- isTRUE(feedback)

  render_source <- if_null_default(
    filter@extra$render_source,
    if_null_default(cohort$attributes$render_source, "auto")
  )

  # Domain mode only applies to a cohort wired through cb_server(), which always
  # records a `render_source` attribute. A bare cohort (e.g. in unit tests or
  # programmatic use) has no render attributes and must default to stats mode,
  # even though its resolved stats are NULL and feedback FALSE.
  configured <- "render_source" %in% names(cohort$attributes) ||
    !is.null(filter@extra$render_source) ||
    !is.null(filter@extra$stats) ||
    !is.null(filter@extra$feedback)

  mode <- if (configured && is.null(stats) && !feedback) "domain" else "stats"

  list(
    mode = mode,
    render_source = render_source,
    stats = stats,
    feedback = feedback
  )
}

# Validate that every filter in every step of the cohort has a declared domain.
#
# render_source = "domain" renders inputs from each filter's `domain`; a filter
# without one has nothing to render. We error eagerly (at cb_server() time)
# rather than per-render so misconfiguration surfaces immediately, while still
# allowing any propagate_domains mode (including "none") as long as domains are
# present. Filters can set their domain explicitly or receive it via propagation
# / step copying.
validate_domains_present <- function(cohort) {
  step_ids <- names(cohort$get_step())
  missing <- list()
  for (step_id in step_ids) {
    step <- cohort$get_step(step_id)
    for (filter in step$filters) {
      if (is.null(cohortBuilder::filter_domain(filter))) {
        missing[[length(missing) + 1]] <- sprintf(
          "step %s / filter '%s'", step_id, filter@id
        )
      }
    }
  }

  if (length(missing) > 0) {
    stop(
      "`render_source = \"domain\"` requires every filter to declare a domain, ",
      "but the following have none: ",
      paste(unlist(missing), collapse = ", "), ". ",
      "Set a `domain` on each filter (e.g. ",
      "`filter(\"discrete\", ..., domain = c(...))`), or use ",
      "`render_source = \"auto\"`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Notify (once per filter) that a filter in domain mode has no domain to render.
warn_no_domain <- function(filter_id) {
  msg <- glue::glue(
    "Filter '{filter_id}': statistics are disabled and no domain is available; ",
    "nothing to render."
  )
  notify_user(msg, type = "warning", id = paste0("no_domain_", filter_id))
}

# Notify (once per filter) that render_source = "domain" cannot apply because the
# filter has no domain, so it falls back to statistics.
inform_domain_fallback <- function(filter_id) {
  msg <- glue::glue(
    "Filter '{filter_id}': render_source = 'domain' requested but no domain is ",
    "available; using statistics instead."
  )
  notify_user(msg, type = "message", id = paste0("domain_fallback_", filter_id))
}

# Route a user-facing message to a Shiny notification when a session is active,
# otherwise to the R console (warning or message). De-duplicated per `id` within
# a session via the session's userData.
notify_user <- function(msg, type = c("warning", "message"), id = NULL) {
  type <- match.arg(type)
  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {
    seen <- session$userData$cb_notified
    if (is.null(seen)) {
      seen <- character(0)
    }
    if (!is.null(id) && id %in% seen) {
      return(invisible(NULL))
    }
    if (!is.null(id)) {
      session$userData$cb_notified <- c(seen, id)
    }
    shiny::showNotification(
      msg,
      type = if (type == "warning") "warning" else "message"
    )
    return(invisible(NULL))
  }

  if (type == "warning") {
    rlang::warn(as.character(msg))
  } else {
    rlang::inform(as.character(msg))
  }
  invisible(NULL)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
