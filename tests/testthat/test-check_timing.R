# tests/testthat/test-check_timing.R
# Tests for check_survey_duration(), check_collection_window(),
# check_future_dates()

# -- check_survey_duration -----------------------------------------------------
test_that("check_survey_duration flags too short and too long surveys", {
  df <- data.frame(
    hh_id    = paste0("H", 1:6),
    duration = c(5, 15, 30, 60, 121, 200),
    stringsAsFactors = FALSE
  )

  res <- check_survey_duration(df, "hh_id", "duration",
                               min_duration = 10, max_duration = 120)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F01_survey_duration")
  expect_equal(res$check_category, "timing")
  expect_equal(res$severity, "warning")
  # H1 (5 min) too short, H5 (121 min) and H6 (200 min) too long
  expect_equal(res$n_flagged, 3L)
  expect_true("H1" %in% res$flagged_ids)
  expect_true("H5" %in% res$flagged_ids)
  expect_true("H6" %in% res$flagged_ids)
})

test_that("check_survey_duration returns zero for normal durations", {
  df <- data.frame(
    hh_id    = paste0("H", 1:4),
    duration = c(15, 30, 45, 60),
    stringsAsFactors = FALSE
  )

  res <- check_survey_duration(df, "hh_id", "duration",
                               min_duration = 10, max_duration = 120)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_survey_duration flag_reason includes expected range", {
  df <- data.frame(hh_id = "H1", duration = 3, stringsAsFactors = FALSE)
  res <- check_survey_duration(df, "hh_id", "duration",
                               min_duration = 15, max_duration = 120)
  expect_true(grepl("15", res$flag_reason[1]))
  expect_true(grepl("120", res$flag_reason[1]))
})

test_that("check_survey_duration summary_stat has median and mean", {
  df <- data.frame(
    hh_id    = paste0("H", 1:5),
    duration = c(20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )
  res <- check_survey_duration(df, "hh_id", "duration")
  expect_true("median_duration" %in% names(res$summary_stat))
  expect_true("mean_duration" %in% names(res$summary_stat))
})

# -- check_collection_window ---------------------------------------------------
test_that("check_collection_window flags dates outside the window", {
  df <- data.frame(
    hh_id = paste0("H", 1:5),
    date  = as.Date(c("2024-12-31", "2025-01-15", "2025-02-01",
                       "2025-03-15", "2025-04-01")),
    stringsAsFactors = FALSE
  )

  res <- check_collection_window(df, "hh_id", "date",
                                 start_date = "2025-01-15",
                                 end_date = "2025-03-15")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F02_collection_window")
  expect_equal(res$severity, "error")
  # H1 (before window) and H5 (after window)
  expect_equal(res$n_flagged, 2L)
  expect_true("H1" %in% res$flagged_ids)
  expect_true("H5" %in% res$flagged_ids)
})

test_that("check_collection_window returns zero when all dates in window", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    date  = as.Date(c("2025-01-15", "2025-02-01", "2025-03-15")),
    stringsAsFactors = FALSE
  )

  res <- check_collection_window(df, "hh_id", "date",
                                 start_date = "2025-01-15",
                                 end_date = "2025-03-15")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_collection_window summary_stat reports n_before and n_after", {
  df <- data.frame(
    hh_id = paste0("H", 1:4),
    date  = as.Date(c("2024-12-01", "2024-12-15",
                       "2025-02-01", "2025-06-01")),
    stringsAsFactors = FALSE
  )

  res <- check_collection_window(df, "hh_id", "date",
                                 start_date = "2025-01-01",
                                 end_date = "2025-03-31")
  expect_equal(res$summary_stat$n_before, 2L)
  expect_equal(res$summary_stat$n_after, 1L)
})

test_that("check_collection_window handles NA dates", {
  df <- data.frame(
    hh_id = c("H1", "H2", "H3"),
    date  = as.Date(c(NA, "2025-02-01", "2025-07-01")),
    stringsAsFactors = FALSE
  )

  res <- check_collection_window(df, "hh_id", "date",
                                 start_date = "2025-01-01",
                                 end_date = "2025-03-31")
  # Only H3 flagged; NA is not flagged
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H3")
})

# -- check_future_dates -------------------------------------------------------
test_that("check_future_dates flags dates in the future", {
  ref_date <- as.Date("2025-06-15")
  df <- data.frame(
    hh_id = paste0("H", 1:4),
    date  = as.Date(c("2025-06-14", "2025-06-15", "2025-06-16", "2025-12-31")),
    stringsAsFactors = FALSE
  )

  res <- check_future_dates(df, "hh_id", "date", reference_date = ref_date)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F03_future_date")
  expect_equal(res$severity, "error")
  # H3 and H4 are in the future
  expect_equal(res$n_flagged, 2L)
  expect_true("H3" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_future_dates returns zero when no future dates", {
  ref_date <- as.Date("2025-12-31")
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    date  = as.Date(c("2025-01-01", "2025-06-15", "2025-12-31")),
    stringsAsFactors = FALSE
  )

  res <- check_future_dates(df, "hh_id", "date", reference_date = ref_date)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_future_dates handles NA dates gracefully", {
  ref_date <- as.Date("2025-06-15")
  df <- data.frame(
    hh_id = c("H1", "H2"),
    date  = as.Date(c(NA, "2025-06-14")),
    stringsAsFactors = FALSE
  )

  res <- check_future_dates(df, "hh_id", "date", reference_date = ref_date)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_future_dates flag_reason mentions 'future'", {
  ref_date <- as.Date("2025-01-01")
  df <- data.frame(hh_id = "H1", date = as.Date("2025-12-31"),
                   stringsAsFactors = FALSE)
  res <- check_future_dates(df, "hh_id", "date", reference_date = ref_date)
  expect_true(grepl("future", res$flag_reason[1], ignore.case = TRUE))
})
