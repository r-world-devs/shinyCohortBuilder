# Tests for internal filter helper functions accessed via :::

test_that("extract_selected_value returns names when value is NA", {
  stats <- c("A" = 10, "B" = 20, "C" = 30)
  result <- shinyCohortBuilder:::extract_selected_value(NA, stats, reset = FALSE)
  expect_equal(result, c("A", "B", "C"))
})

test_that("extract_selected_value returns names when reset is TRUE", {
  stats <- c("A" = 10, "B" = 20)
  result <- shinyCohortBuilder:::extract_selected_value("A", stats, reset = TRUE)
  expect_equal(result, c("A", "B"))
})

test_that("extract_selected_value returns NULL when value is NULL", {
  stats <- c("A" = 10, "B" = 20)
  result <- shinyCohortBuilder:::extract_selected_value(NULL, stats, reset = FALSE)
  expect_null(result)
})

test_that("extract_selected_value intersects with available values", {
  stats <- c("A" = 10, "B" = 20)
  result <- shinyCohortBuilder:::extract_selected_value(c("A", "X"), stats, reset = FALSE)
  expect_equal(result, "A")
})

test_that("extract_selected_value returns value when all match", {
  stats <- c("A" = 10, "B" = 20, "C" = 30)
  result <- shinyCohortBuilder:::extract_selected_value(c("A", "C"), stats, reset = FALSE)
  expect_equal(result, c("A", "C"))
})

test_that("extract_selected_range returns parent_range on reset", {
  result <- shinyCohortBuilder:::extract_selected_range(c(5, 15), c(0, 20), reset = TRUE)
  expect_equal(result, c(0, 20))
})

test_that("extract_selected_range returns parent_range when range is NA", {
  result <- shinyCohortBuilder:::extract_selected_range(NA, c(0, 20), reset = FALSE)
  expect_equal(result, c(0, 20))
})

test_that("extract_selected_range clips range to parent bounds", {
  result <- shinyCohortBuilder:::extract_selected_range(c(-5, 25), c(0, 20), reset = FALSE)
  expect_equal(result, c(0, 20))
})

test_that("extract_selected_range preserves valid range", {
  result <- shinyCohortBuilder:::extract_selected_range(c(5, 15), c(0, 20), reset = FALSE)
  expect_equal(result, c(5, 15))
})

test_that("extract_selected_range clips lower bound only", {
  result <- shinyCohortBuilder:::extract_selected_range(c(-5, 15), c(0, 20), reset = FALSE)
  expect_equal(result, c(0, 15))
})

test_that("extract_selected_range clips upper bound only", {
  result <- shinyCohortBuilder:::extract_selected_range(c(5, 25), c(0, 20), reset = FALSE)
  expect_equal(result, c(5, 20))
})

test_that("sf_id concatenates step and filter id", {
  result <- shinyCohortBuilder:::sf_id("1", "age")
  expect_equal(result, "1-age")

  result2 <- shinyCohortBuilder:::sf_id("3", "species_filter")
  expect_equal(result2, "3-species_filter")
})

test_that("if_null_default returns value when not NULL", {
  result <- shinyCohortBuilder:::if_null_default("actual", "default")
  expect_equal(result, "actual")
})

test_that("if_null_default returns default when NULL", {
  result <- shinyCohortBuilder:::if_null_default(NULL, "default")
  expect_equal(result, "default")
})

test_that("is_none returns TRUE for 'none'", {
  expect_true(shinyCohortBuilder:::is_none("none"))
})

test_that("is_none returns FALSE for other values", {
  expect_false(shinyCohortBuilder:::is_none("local"))
  expect_false(shinyCohortBuilder:::is_none("global"))
  expect_false(shinyCohortBuilder:::is_none(NULL))
  expect_false(shinyCohortBuilder:::is_none(TRUE))
})

test_that("modify_list returns y when x is NULL", {
  result <- shinyCohortBuilder:::modify_list(NULL, list(a = 1))
  expect_equal(result, list(a = 1))
})

test_that("modify_list merges two lists", {
  result <- shinyCohortBuilder:::modify_list(
    list(a = 1, b = 2),
    list(b = 3, c = 4)
  )
  expect_equal(result$a, 1)
  expect_equal(result$b, 3)
  expect_equal(result$c, 4)
})

test_that("modify_list keeps NULL values", {
  result <- shinyCohortBuilder:::modify_list(
    list(a = 1),
    list(a = NULL)
  )
  expect_true("a" %in% names(result))
  expect_null(result$a)
})

test_that("get_matching_vals returns original when selected is NA", {
  result <- shinyCohortBuilder:::get_matching_vals(NA, "A,B,C")
  expect_equal(result, "A,B,C")
})

test_that("get_matching_vals returns empty string for empty input", {
  result <- shinyCohortBuilder:::get_matching_vals("", "A,B,C")
  expect_equal(result, "")
})

test_that("get_matching_vals intersects when some values are missing", {
  result <- shinyCohortBuilder:::get_matching_vals("A,D", "A,B,C")
  expect_equal(result, "A")
})

test_that("get_matching_vals returns selected when all match", {
  result <- shinyCohortBuilder:::get_matching_vals("A,B", "A,B,C")
  expect_equal(result, "A,B")
})

test_that("get_matching_vals returns original on reset", {
  result <- shinyCohortBuilder:::get_matching_vals(NA, "X,Y,Z", reset = TRUE)
  expect_equal(result, "X,Y,Z")
})

test_that("get_n_matching_vals counts matching values", {
  result <- shinyCohortBuilder:::get_n_matching_vals("A,B", "A,B,C")
  expect_equal(result, 2)
})

test_that("get_n_matching_vals returns total when NA", {
  result <- shinyCohortBuilder:::get_n_matching_vals(NA, "A,B,C")
  expect_equal(result, 3)
})

test_that("extend_stats fills missing entries with NA", {
  result <- shinyCohortBuilder:::extend_stats(
    list("A" = 5),
    list("A" = 10, "B" = 20)
  )
  expect_equal(result$A, 5)
  expect_equal(result$B, 0)
})

test_that("extend_stats inherits parent for specified keys", {
  result <- shinyCohortBuilder:::extend_stats(
    list("A" = 5),
    list("A" = 10, "B" = 20),
    inherit_parent = "B"
  )
  expect_equal(result$B, 20)
})

test_that("extend_stats preserves order from parent", {
  result <- shinyCohortBuilder:::extend_stats(
    list("B" = 5, "A" = 3),
    list("A" = 10, "B" = 20)
  )
  expect_equal(names(result), c("A", "B"))
})
