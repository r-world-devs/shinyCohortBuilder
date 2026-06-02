test_that(".pre_post_stats returns shiny.tag with correct structure", {
  result <- .pre_post_stats(5, 10, "books")
  expect_s3_class(result, "shiny.tag")
  rendered <- as.character(result)
  expect_true(grepl("5", rendered))
  expect_true(grepl("10", rendered))
  expect_true(grepl("books", rendered))
})

test_that(".pre_post_stats with brackets wraps in parentheses", {
  result <- .pre_post_stats(5, 10, "books", brackets = TRUE)
  rendered <- as.character(result)
  expect_true(grepl("\\(", rendered))
  expect_true(grepl("\\)", rendered))
})

test_that(".pre_post_stats without brackets has no parentheses", {
  result <- .pre_post_stats(5, 10, brackets = FALSE)
  rendered <- as.character(result)
  # The only parentheses should be absent (no bracket wrapping)
  expect_false(grepl("\\(.*5.*\\)", rendered))
})

test_that(".pre_post_stats with percent shows percentage", {
  result <- .pre_post_stats(5, 10, "books", percent = TRUE)
  rendered <- as.character(result)
  expect_true(grepl("50%", rendered))
})

test_that(".pre_post_stats with stats = 'pre' shows only previous", {
  result <- .pre_post_stats(5, 10, stats = "pre")
  rendered <- as.character(result)
  expect_true(grepl("10", rendered))
  # post value should be in cb_delayed span only if stats includes "post"
  expect_false(grepl("cb_delayed", rendered))
})

test_that(".pre_post_stats with stats = 'post' shows only current", {
  result <- .pre_post_stats(5, 10, stats = "post")
  rendered <- as.character(result)
  expect_true(grepl("cb_delayed", rendered))
  expect_true(grepl("5", rendered))
})

test_that(".pre_post_stats with stats = NULL shows nothing", {
  result <- .pre_post_stats(5, 10, stats = NULL)
  rendered <- as.character(result)
  # Should not contain the actual stats values in stat-related tags
  expect_false(grepl("cb_delayed", rendered))
})

test_that(".pre_post_stats_text returns character", {
  result <- .pre_post_stats_text(5, 10, "books")
  expect_type(result, "character")
})

test_that(".pre_post_stats_text with brackets includes parentheses", {
  result <- .pre_post_stats_text(5, 10, "books", brackets = TRUE)
  expect_true(grepl("\\(", result))
  expect_true(grepl("\\)", result))
})

test_that(".pre_post_stats_text with percent shows percentage", {
  result <- .pre_post_stats_text(5, 10, "books", percent = TRUE)
  expect_true(grepl("50%", result))
})

test_that(".pre_post_stats_text with stats = 'pre' only", {

  result <- .pre_post_stats_text(5, 10, "books", stats = "pre")
  expect_true(grepl("10", result))
  expect_false(grepl("cb_delayed", result))
})

test_that(".pre_post_stats_text with stats = 'post' only", {
  result <- .pre_post_stats_text(5, 10, "books", stats = "post")
  expect_true(grepl("cb_delayed", result))
  expect_true(grepl("5", result))
})

test_that(".pre_post_stats_text supports vector arguments", {
  result <- .pre_post_stats_text(5:6, 10:11, "items")
  expect_true(length(result) == 2)
})

test_that(".pre_post_stats_text percent rounds correctly", {
  result <- .pre_post_stats_text(3, 7, "items", percent = TRUE)
  expect_true(grepl("43%", result))
})

test_that(".pre_post_stats uses cb_delayed class for current value", {
  result <- .pre_post_stats(8, 20)
  rendered <- as.character(result)
  expect_true(grepl("cb_delayed", rendered))
  expect_true(grepl("8", rendered))
})
