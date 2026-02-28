# tests/testthat/test-check_missing.R
# Tests for check_missing_by_variable(), check_missing_by_enumerator(),
# check_all_missing_variables(), check_skip_pattern()

# -- check_missing_by_variable -------------------------------------------------
test_that("check_missing_by_variable flags columns exceeding threshold", {
  df <- data.frame(
    hh_id  = paste0("H", 1:20),
    col_a  = c(rep(NA, 5), 6:20),   # 25% missing
    col_b  = c(rep(NA, 1), 2:20),   # 5% missing
    col_c  = 1:20,                   # 0% missing
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_variable(df, "hh_id", threshold = 0.05)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "C01_missing_by_variable")
  expect_equal(res$check_category, "completeness")
  # col_a (25%) should be flagged, col_b (5%) is at threshold not above
  expect_true("col_a" %in% res$flagged_ids)
  expect_true(res$n_flagged >= 1L)
})

test_that("check_missing_by_variable respects exclude_cols", {
  df <- data.frame(
    hh_id = paste0("H", 1:10),
    notes = rep(NA, 10),   # 100% missing but excluded
    val   = 1:10,
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_variable(df, "hh_id", threshold = 0.05,
                                   exclude_cols = "notes")
  # notes should be excluded from check
  expect_false("notes" %in% res$flagged_ids)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_missing_by_variable returns zero with complete data", {
  df <- data.frame(
    hh_id = paste0("H", 1:10),
    col_a = 1:10,
    col_b = 11:20,
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_variable(df, "hh_id", threshold = 0.05)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_missing_by_variable severity escalates for high missing rate", {
  df <- data.frame(
    hh_id = paste0("H", 1:10),
    col_a = c(rep(NA, 8), 9, 10),  # 80% missing
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_variable(df, "hh_id", threshold = 0.05)
  expect_equal(res$severity, "error")  # >20% => error
})

# -- check_missing_by_enumerator -----------------------------------------------
test_that("check_missing_by_enumerator flags enumerators with high missing rate", {
  # Enumerator E3 has lots of missing values
  df <- data.frame(
    hh_id   = paste0("H", 1:12),
    enum_id = rep(c("E1", "E2", "E3"), each = 4),
    col_a   = c(1:8, NA, NA, NA, NA),
    col_b   = c(1:8, NA, NA, NA, NA),
    col_c   = c(1:8, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_enumerator(df, "hh_id", "enum_id", threshold_sd = 1)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "C02_missing_by_enumerator")
  expect_equal(res$severity, "warning")
  # E3 has 100% missing vs E1 and E2 at 0%
  expect_true("E3" %in% res$flagged_ids)
})

test_that("check_missing_by_enumerator returns zero with uniform missing rates", {
  df <- data.frame(
    hh_id   = paste0("H", 1:6),
    enum_id = rep(c("E1", "E2"), each = 3),
    col_a   = c(1, NA, 3, 4, NA, 6),
    stringsAsFactors = FALSE
  )

  res <- check_missing_by_enumerator(df, "hh_id", "enum_id", threshold_sd = 2)
  expect_equal(res$n_flagged, 0L)
})

# -- check_all_missing_variables -----------------------------------------------
test_that("check_all_missing_variables flags entirely missing columns", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    good  = 1:5,
    empty = rep(NA, 5),
    also_empty = rep(NA_real_, 5),
    stringsAsFactors = FALSE
  )

  res <- check_all_missing_variables(df)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "C04_all_missing_variable")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 2L)
  expect_true("empty" %in% res$flagged_ids)
  expect_true("also_empty" %in% res$flagged_ids)
})

test_that("check_all_missing_variables returns zero with no empty columns", {
  df <- data.frame(
    hh_id = 1:5,
    col_a = 1:5,
    col_b = letters[1:5],
    stringsAsFactors = FALSE
  )

  res <- check_all_missing_variables(df)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_all_missing_variables reports n_total as ncol", {
  df <- data.frame(a = 1:3, b = NA, c = NA)
  res <- check_all_missing_variables(df)
  expect_equal(res$n_total, 3L)
})

# -- check_skip_pattern --------------------------------------------------------
test_that("check_skip_pattern flags violated skip logic (expect_child_na = TRUE)", {
  df <- data.frame(
    hh_id         = paste0("H", 1:5),
    has_livestock  = c("yes", "no", "no", "yes", "no"),
    n_cattle       = c(5, NA, 3, 10, NA),  # row 3 violates: no livestock but has cattle
    stringsAsFactors = FALSE
  )

  res <- check_skip_pattern(df, "hh_id",
                            parent_col = "has_livestock",
                            child_col = "n_cattle",
                            parent_value = "no",
                            expect_child_na = TRUE)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "C03_skip_pattern")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H3")
})

test_that("check_skip_pattern flags violated skip logic (expect_child_na = FALSE)", {
  df <- data.frame(
    hh_id     = paste0("H", 1:4),
    is_farmer = c(1, 1, 0, 1),
    crop_type = c("maize", NA, NA, "rice"),  # row 2: is_farmer=1 but crop_type=NA
    stringsAsFactors = FALSE
  )

  res <- check_skip_pattern(df, "hh_id",
                            parent_col = "is_farmer",
                            child_col = "crop_type",
                            parent_value = 1,
                            expect_child_na = FALSE)

  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})

test_that("check_skip_pattern returns zero when logic is consistent", {
  df <- data.frame(
    hh_id         = paste0("H", 1:4),
    has_livestock  = c("yes", "no", "no", "yes"),
    n_cattle       = c(5, NA, NA, 10),
    stringsAsFactors = FALSE
  )

  res <- check_skip_pattern(df, "hh_id",
                            parent_col = "has_livestock",
                            child_col = "n_cattle",
                            parent_value = "no",
                            expect_child_na = TRUE)

  expect_equal(res$n_flagged, 0L)
})
