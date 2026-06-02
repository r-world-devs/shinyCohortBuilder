# Shared helpers for shinytest2 UI tests

skip_on_ci <- function() {
  if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITLAB_CI"))) {
    testthat::skip("UI tests skipped on CI")
  }
}

skip_without_screenshot_tests <- function() {
  if (!nzchar(Sys.getenv("SCREENSHOT_TESTS"))) {
    testthat::skip("Set SCREENSHOT_TESTS=1 to run screenshot tests")
  }
}

skip_if_screenshot_only <- function() {
  if (nzchar(Sys.getenv("SCREENSHOT_ONLY"))) {
    testthat::skip("SCREENSHOT_ONLY mode — skipping non-screenshot tests")
  }
}

# Disable animations and transitions for deterministic screenshots
disable_animations_js <- paste0(
  "var s = document.createElement('style');",
  "s.textContent = [",
  "  '*, *::before, *::after { animation: none !important; transition: none !important; }',",
  "  '.girafe_container_std svg { display: none !important; }'",
  "].join('\\n');",
  "document.head.appendChild(s);"
)
