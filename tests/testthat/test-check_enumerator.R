# tests/testthat/test-check_enumerator.R
# Tests for check_enumerator_productivity(), check_enumerator_duration(),
# check_dk_rate(), check_enumerator_time_gap()

# -- check_enumerator_productivity ---------------------------------------------
test_that("check_enumerator_productivity flags over-limit days", {
  # E1 does 3 surveys on day 1, E2 does 4 on day 1
  df <- data.frame(
    hh_id   = paste0("H", 1:7),
    enum_id = c("E1", "E1", "E1", "E2", "E2", "E2", "E2"),
    date    = as.Date(rep("2025-02-01", 7)),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_productivity(df, "hh_id", "enum_id", "date",
                                       max_daily = 3)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "B01_enumerator_productivity")
  expect_equal(res$severity, "warning")
  # E2 has 4 surveys, exceeding max_daily=3
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "E2")
})

test_that("check_enumerator_productivity returns zero when under limit", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    enum_id = c("E1", "E1", "E2", "E2"),
    date    = as.Date(rep("2025-02-01", 4)),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_productivity(df, "hh_id", "enum_id", "date",
                                       max_daily = 10)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_enumerator_productivity handles multiple days", {
  # E1: 2 on day1, 2 on day2 = ok at max_daily=2
  # E2: 3 on day1 = exceeds max_daily=2
  df <- data.frame(
    hh_id   = paste0("H", 1:7),
    enum_id = c("E1", "E1", "E1", "E1", "E2", "E2", "E2"),
    date    = as.Date(c("2025-02-01", "2025-02-01",
                        "2025-02-02", "2025-02-02",
                        "2025-02-01", "2025-02-01", "2025-02-01")),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_productivity(df, "hh_id", "enum_id", "date",
                                       max_daily = 2)
  expect_true("E2" %in% res$flagged_ids)
})

# -- check_enumerator_duration -------------------------------------------------
test_that("check_enumerator_duration flags unusually fast enumerators", {
  # E1 and E2 average ~50 min, E3 averages ~10 min (suspiciously fast)
  df <- data.frame(
    hh_id    = paste0("H", 1:9),
    enum_id  = rep(c("E1", "E2", "E3"), each = 3),
    duration = c(50, 55, 45, 48, 52, 50, 10, 12, 8),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_duration(df, "hh_id", "enum_id", "duration",
                                   threshold_sd = 1)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "B02_enumerator_duration")
  expect_equal(res$severity, "warning")
  expect_true("E3" %in% res$flagged_ids)
})

test_that("check_enumerator_duration returns zero with similar durations", {
  df <- data.frame(
    hh_id    = paste0("H", 1:6),
    enum_id  = rep(c("E1", "E2"), each = 3),
    duration = c(50, 55, 45, 48, 52, 50),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_duration(df, "hh_id", "enum_id", "duration",
                                   threshold_sd = 2)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_enumerator_duration handles single enumerator (sd=0)", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    enum_id  = rep("E1", 3),
    duration = c(30, 35, 40),
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_duration(df, "hh_id", "enum_id", "duration")
  expect_equal(res$n_flagged, 0L)
})

# -- check_dk_rate -------------------------------------------------------------
test_that("check_dk_rate flags enumerators with high DK rate", {
  # E1: no DK, E2: no DK, E3: all DK
  df <- data.frame(
    hh_id   = paste0("H", 1:9),
    enum_id = rep(c("E1", "E2", "E3"), each = 3),
    q1      = c("yes", "no", "yes", "yes", "no", "no", "dk", "dk", "dk"),
    q2      = c(1, 2, 3, 4, 5, 6, -99, -88, -77),
    stringsAsFactors = FALSE
  )

  res <- check_dk_rate(df, "hh_id", "enum_id", dk_value = "dk",
                       threshold_sd = 1)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "B03_dk_rate")
  expect_equal(res$severity, "warning")
  expect_true("E3" %in% res$flagged_ids)
})

test_that("check_dk_rate returns zero when no DK responses", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    enum_id = rep(c("E1", "E2"), each = 2),
    q1      = c("yes", "no", "yes", "no"),
    q2      = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  res <- check_dk_rate(df, "hh_id", "enum_id")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_dk_rate respects check_cols parameter", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    enum_id = rep(c("E1", "E2"), each = 2),
    q1      = c("dk", "dk", "yes", "no"),
    q2      = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  # Only check q2 (no DK), so no flags
  res <- check_dk_rate(df, "hh_id", "enum_id", check_cols = "q2")
  expect_equal(res$n_flagged, 0L)
})

# -- check_enumerator_time_gap ------------------------------------------------
test_that("check_enumerator_time_gap flags suspiciously short gaps", {
  base_time <- as.POSIXct("2025-02-01 09:00:00")
  df <- data.frame(
    hh_id      = paste0("H", 1:5),
    enum_id    = rep("E1", 5),
    start_time = base_time + c(0, 120, 180, 182, 600),  # gaps: NA, 2min, 1min, 0.03min, 6.97min
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_time_gap(df, "hh_id", "enum_id", "start_time",
                                   min_gap_minutes = 5)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "B08_enumerator_time_gap")
  expect_equal(res$severity, "warning")
  # H2 (2min gap), H3 (1min gap), H4 (0.03min gap) should be flagged
  expect_true(res$n_flagged >= 2L)
  expect_true("H2" %in% res$flagged_ids || "H3" %in% res$flagged_ids)
})

test_that("check_enumerator_time_gap returns zero with adequate gaps", {
  base_time <- as.POSIXct("2025-02-01 09:00:00")
  df <- data.frame(
    hh_id      = paste0("H", 1:3),
    enum_id    = rep("E1", 3),
    start_time = base_time + c(0, 600, 1200),  # 10min gaps
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_time_gap(df, "hh_id", "enum_id", "start_time",
                                   min_gap_minutes = 5)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_enumerator_time_gap handles multiple enumerators", {
  base_time <- as.POSIXct("2025-02-01 09:00:00")
  df <- data.frame(
    hh_id      = paste0("H", 1:4),
    enum_id    = c("E1", "E1", "E2", "E2"),
    start_time = base_time + c(0, 60, 0, 600),  # E1: 1min gap, E2: 10min gap
    stringsAsFactors = FALSE
  )

  res <- check_enumerator_time_gap(df, "hh_id", "enum_id", "start_time",
                                   min_gap_minutes = 5)
  # Only E1's second survey should be flagged
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})
