# tests/testthat/test-check_outliers.R
# Tests for check_outliers_iqr(), check_outliers_zscore(), check_outliers_mad(),
# check_hard_range(), check_negative_values()

# -- check_outliers_iqr --------------------------------------------------------
test_that("check_outliers_iqr flags known outliers", {
  df <- data.frame(
    hh_id  = paste0("H", 1:10),
    income = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 9999),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_iqr(df, "hh_id", "income", multiplier = 1.5)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "D01_outlier_iqr")
  expect_equal(res$check_category, "outliers")
  expect_equal(res$severity, "warning")
  # 9999 should be flagged as an outlier
  expect_true("H10" %in% res$flagged_ids)
  expect_true(res$n_flagged >= 1L)
})

test_that("check_outliers_iqr returns zero with uniform data", {
  df <- data.frame(
    hh_id = paste0("H", 1:10),
    val   = rep(50, 10),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_iqr(df, "hh_id", "val")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_outliers_iqr handles NA values", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    val   = c(10, 20, NA, 30, 40),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_iqr(df, "hh_id", "val")
  # n_total should exclude NAs
  expect_equal(res$n_total, 4L)
})

test_that("check_outliers_iqr summary_stat contains bounds", {
  df <- data.frame(hh_id = paste0("H", 1:5), val = c(1, 2, 3, 4, 5),
                   stringsAsFactors = FALSE)
  res <- check_outliers_iqr(df, "hh_id", "val")
  expect_true("lower_bound" %in% names(res$summary_stat))
  expect_true("upper_bound" %in% names(res$summary_stat))
  expect_true("IQR" %in% names(res$summary_stat))
})

# -- check_outliers_zscore -----------------------------------------------------
test_that("check_outliers_zscore flags extreme values", {
  # Use varied data so MAD is non-zero; H11 = 5000 is the clear outlier
  df <- data.frame(
    hh_id = paste0("H", 1:11),
    val   = c(45, 48, 50, 52, 55, 47, 53, 49, 51, 46, 5000),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_zscore(df, "hh_id", "val", threshold = 3)

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "D02_outlier_zscore")
  expect_true(res$severity == "warning")
  expect_true("H11" %in% res$flagged_ids)
})

test_that("check_outliers_zscore returns zero when MAD is 0", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    val   = rep(100, 5),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_zscore(df, "hh_id", "val")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_outliers_zscore errors on non-numeric column", {
  df <- data.frame(hh_id = c("A", "B"), val = c("x", "y"),
                   stringsAsFactors = FALSE)
  expect_error(check_outliers_zscore(df, "hh_id", "val"), "numeric")
})

# -- check_outliers_mad --------------------------------------------------------
test_that("check_outliers_mad flags extreme values", {
  # Use varied data so MAD is non-zero; H11 = 5000 is the clear outlier
  df <- data.frame(
    hh_id = paste0("H", 1:11),
    val   = c(45, 48, 50, 52, 55, 47, 53, 49, 51, 46, 5000),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_mad(df, "hh_id", "val", threshold = 3)

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "D03_outlier_mad")
  expect_true(res$severity == "warning")
  expect_true("H11" %in% res$flagged_ids)
})

test_that("check_outliers_mad returns zero when MAD is 0", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    val   = rep(100, 5),
    stringsAsFactors = FALSE
  )

  res <- check_outliers_mad(df, "hh_id", "val")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_outliers_mad summary_stat contains expected fields", {
  df <- data.frame(hh_id = paste0("H", 1:5), val = c(1, 2, 3, 4, 100),
                   stringsAsFactors = FALSE)
  res <- check_outliers_mad(df, "hh_id", "val")
  expect_true("median" %in% names(res$summary_stat))
  expect_true("mad" %in% names(res$summary_stat))
  expect_true("threshold" %in% names(res$summary_stat))
})

# -- check_hard_range ----------------------------------------------------------
test_that("check_hard_range flags values outside min/max", {
  df <- data.frame(
    hh_id = paste0("H", 1:6),
    age   = c(25, 30, -5, 150, 45, 0),
    stringsAsFactors = FALSE
  )

  res <- check_hard_range(df, "hh_id", "age", min_val = 0, max_val = 120)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "D04_hard_range")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 2L)
  expect_true("H3" %in% res$flagged_ids)   # age = -5
  expect_true("H4" %in% res$flagged_ids)   # age = 150
})

test_that("check_hard_range returns zero when all values in range", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    size  = c(1, 5, 15),
    stringsAsFactors = FALSE
  )

  res <- check_hard_range(df, "hh_id", "size", min_val = 1, max_val = 30)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_hard_range flag_reason describes the violation", {
  df <- data.frame(hh_id = "H1", val = -10, stringsAsFactors = FALSE)
  res <- check_hard_range(df, "hh_id", "val", min_val = 0, max_val = 100)
  expect_true(grepl("below", res$flag_reason[1]))
})

test_that("check_hard_range handles NA values without flagging them", {
  df <- data.frame(hh_id = c("H1", "H2"), val = c(NA, 50),
                   stringsAsFactors = FALSE)
  res <- check_hard_range(df, "hh_id", "val", min_val = 0, max_val = 100)
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 1L)
})

# -- check_negative_values ----------------------------------------------------
test_that("check_negative_values flags negative numbers", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    area  = c(10, 20, -3, 0, -1),
    stringsAsFactors = FALSE
  )

  res <- check_negative_values(df, "hh_id", "area")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "D09_negative_value")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 2L)
  expect_true("H3" %in% res$flagged_ids)
  expect_true("H5" %in% res$flagged_ids)
})

test_that("check_negative_values returns zero with all non-negative values", {
  df <- data.frame(
    hh_id = paste0("H", 1:4),
    count = c(0, 1, 5, 100),
    stringsAsFactors = FALSE
  )

  res <- check_negative_values(df, "hh_id", "count")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_negative_values summary_stat reports min negative", {
  df <- data.frame(hh_id = c("H1", "H2"), val = c(-10, -5),
                   stringsAsFactors = FALSE)
  res <- check_negative_values(df, "hh_id", "val")
  expect_equal(res$summary_stat$min_value, -10)
})
