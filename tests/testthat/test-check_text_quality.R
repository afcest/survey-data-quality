# tests/testthat/test-check_text_quality.R
# Tests for check_other_specify(), check_text_length(),
# check_text_duplicates(), check_name_format(), check_phone_format()

# =============================================================================
# check_other_specify
# =============================================================================

test_that("check_other_specify flags non-empty other values", {
  data <- data.frame(
    id        = c("1", "2", "3", "4"),
    crop_other = c("maize", "", NA, "sorghum"),
    stringsAsFactors = FALSE
  )

  res <- check_other_specify(data, "id", "crop_other")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "O01_other_specify")
  expect_equal(res$severity, "info")
  expect_equal(res$n_flagged, 2L)
  expect_true(all(c("1", "4") %in% res$flagged_ids))
})

test_that("check_other_specify returns 0 when all empty or NA", {
  data <- data.frame(
    id        = c("1", "2"),
    crop_other = c("", NA),
    stringsAsFactors = FALSE
  )

  res <- check_other_specify(data, "id", "crop_other")

  expect_equal(res$n_flagged, 0L)
})

test_that("check_other_specify builds value counts in summary", {
  data <- data.frame(
    id         = c("1", "2", "3"),
    crop_other = c("maize", "maize", "rice"),
    stringsAsFactors = FALSE
  )

  res <- check_other_specify(data, "id", "crop_other")

  expect_equal(res$n_flagged, 3L)
  vc <- res$summary_stat$value_counts
  expect_true("maize" %in% vc$value)
  maize_row <- vc[vc$value == "maize", ]
  expect_equal(maize_row$n, 2L)
  expect_equal(res$summary_stat$n_unique_values, 2L)
})

# =============================================================================
# check_text_length
# =============================================================================

test_that("check_text_length flags too short and too long text", {
  data <- data.frame(
    id   = c("1", "2", "3", "4"),
    text = c("ab", "good text", paste(rep("x", 600), collapse = ""), NA),
    stringsAsFactors = FALSE
  )

  res <- check_text_length(data, "id", "text",
                            min_length = 3, max_length = 500)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "O02_text_length")
  expect_equal(res$severity, "warning")
  # "ab" is 2 chars (too short), 600 x's (too long), NA excluded
  expect_equal(res$n_flagged, 2L)
  expect_true("1" %in% res$flagged_ids)
  expect_true("3" %in% res$flagged_ids)
  expect_equal(res$summary_stat$n_too_short, 1L)
  expect_equal(res$summary_stat$n_too_long, 1L)
})

test_that("check_text_length returns 0 for valid text", {
  data <- data.frame(
    id   = c("1", "2"),
    text = c("hello", "world"),
    stringsAsFactors = FALSE
  )

  res <- check_text_length(data, "id", "text",
                            min_length = 3, max_length = 500)

  expect_equal(res$n_flagged, 0L)
})

test_that("check_text_length excludes NA and empty strings", {
  data <- data.frame(
    id   = c("1", "2", "3"),
    text = c(NA, "", "ok text"),
    stringsAsFactors = FALSE
  )

  res <- check_text_length(data, "id", "text",
                            min_length = 3, max_length = 500)

  # NA and "" are excluded, "ok text" is valid
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$n_excluded_na_empty, 2L)
})

# =============================================================================
# check_text_duplicates
# =============================================================================

test_that("check_text_duplicates flags identical long texts", {
  long_text <- paste(rep("word", 5), collapse = " ")
  data <- data.frame(
    id   = c("1", "2", "3"),
    text = c(long_text, long_text, "something unique and long enough"),
    stringsAsFactors = FALSE
  )

  res <- check_text_duplicates(data, "id", "text", min_length = 10)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "O03_text_duplicate")
  expect_equal(res$severity, "warning")
  # IDs 1 and 2 share identical text
  expect_equal(res$n_flagged, 2L)
  expect_true(all(c("1", "2") %in% res$flagged_ids))
  expect_equal(res$summary_stat$n_duplicate_groups, 1L)
})

test_that("check_text_duplicates ignores short texts below min_length", {
  data <- data.frame(
    id   = c("1", "2"),
    text = c("hi", "hi"),
    stringsAsFactors = FALSE
  )

  res <- check_text_duplicates(data, "id", "text", min_length = 10)

  # "hi" is only 2 chars, below min_length=10
  expect_equal(res$n_flagged, 0L)
})

test_that("check_text_duplicates returns 0 when all unique", {
  data <- data.frame(
    id   = c("1", "2"),
    text = c("this is a unique text response", "another unique response here"),
    stringsAsFactors = FALSE
  )

  res <- check_text_duplicates(data, "id", "text", min_length = 10)

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$n_duplicate_groups, 0L)
})

# =============================================================================
# check_name_format
# =============================================================================

test_that("check_name_format flags test values, digits, and single chars", {
  data <- data.frame(
    id   = c("1", "2", "3", "4", "5", "6"),
    name = c("test", "12345", "x", "Alice", NA, "asdf"),
    stringsAsFactors = FALSE
  )

  res <- check_name_format(data, "id", "name")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "O04_name_format")
  expect_equal(res$severity, "warning")
  # "test" = test value, "12345" = digits, "x" = single char,
  # NA = missing, "asdf" = test value
  expect_equal(res$n_flagged, 5L)
  # "Alice" is valid
  expect_false("4" %in% res$flagged_ids)
})

test_that("check_name_format returns 0 for valid names", {
  data <- data.frame(
    id   = c("1", "2"),
    name = c("Alice", "Bob"),
    stringsAsFactors = FALSE
  )

  res <- check_name_format(data, "id", "name")

  expect_equal(res$n_flagged, 0L)
})

test_that("check_name_format summary provides breakdown counts", {
  data <- data.frame(
    id   = c("1", "2", "3", "4"),
    name = c(NA, "123", "test", "a"),
    stringsAsFactors = FALSE
  )

  res <- check_name_format(data, "id", "name")

  expect_equal(res$summary_stat$n_missing, 1L)
  expect_equal(res$summary_stat$n_digits_only, 1L)
  expect_equal(res$summary_stat$n_test_value, 1L)
  expect_equal(res$summary_stat$n_single_char, 1L)
})

# =============================================================================
# check_phone_format
# =============================================================================

test_that("check_phone_format flags too few digits", {
  data <- data.frame(
    id    = c("1", "2"),
    phone = c("123", "12345678"),
    stringsAsFactors = FALSE
  )

  res <- check_phone_format(data, "id", "phone",
                             min_digits = 8, max_digits = 15)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "O05_phone_format")
  expect_equal(res$severity, "warning")
  # "123" has 3 digits < 8
  expect_equal(res$n_flagged, 1L)
  expect_true("1" %in% res$flagged_ids)
  expect_equal(res$summary_stat$n_too_few_digits, 1L)
})

test_that("check_phone_format flags too many digits", {
  data <- data.frame(
    id    = c("1"),
    phone = c("1234567890123456"),
    stringsAsFactors = FALSE
  )

  res <- check_phone_format(data, "id", "phone",
                             min_digits = 8, max_digits = 15)

  # 16 digits > 15
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$summary_stat$n_too_many_digits, 1L)
})

test_that("check_phone_format validates country code prefix", {
  data <- data.frame(
    id    = c("1", "2"),
    phone = c("+22670123456", "70123456"),
    stringsAsFactors = FALSE
  )

  res <- check_phone_format(data, "id", "phone",
                             country_code = "+226",
                             min_digits = 8, max_digits = 15)

  # "70123456" does not start with +226 or 226
  expect_equal(res$summary_stat$n_bad_prefix, 1L)
})

test_that("check_phone_format returns 0 for valid phones", {
  data <- data.frame(
    id    = c("1", "2"),
    phone = c("+22670123456", "+22675987654"),
    stringsAsFactors = FALSE
  )

  res <- check_phone_format(data, "id", "phone",
                             country_code = "+226",
                             min_digits = 8, max_digits = 15)

  expect_equal(res$n_flagged, 0L)
})

test_that("check_phone_format excludes NA and empty phones", {
  data <- data.frame(
    id    = c("1", "2"),
    phone = c(NA, ""),
    stringsAsFactors = FALSE
  )

  res <- check_phone_format(data, "id", "phone",
                             min_digits = 8, max_digits = 15)

  # NA and empty are not flagged as violations
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$n_na_or_empty, 2L)
})
