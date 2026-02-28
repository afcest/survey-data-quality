# tests/testthat/test-check_logic.R
# Tests for logic check functions (Phase 2)

# -- check_hh_composition -----------------------------------------------------
test_that("check_hh_composition flags implausible household size", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    hh_size = c(5, 0, 35, 10),
    stringsAsFactors = FALSE
  )

  res <- check_hh_composition(df, "hh_id", "hh_size")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "G01_hh_composition")
  expect_equal(res$check_category, "logic")
  expect_equal(res$severity, "warning")
  # H2 (size=0 < 1) and H3 (size=35 > 30) are implausible
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H3" %in% res$flagged_ids)
})

test_that("check_hh_composition flags roster mismatch", {
  df <- data.frame(
    hh_id        = paste0("H", 1:3),
    hh_size      = c(5, 4, 6),
    roster_count = c(5, 3, 6),
    stringsAsFactors = FALSE
  )

  res <- check_hh_composition(df, "hh_id", "hh_size",
                               roster_count_col = "roster_count")
  # H2: hh_size=4 != roster_count=3
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})

test_that("check_hh_composition returns 0 for valid data", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    hh_size = c(3, 5, 10),
    stringsAsFactors = FALSE
  )

  res <- check_hh_composition(df, "hh_id", "hh_size")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_hh_composition flags both implausible and mismatch", {
  df <- data.frame(
    hh_id        = c("H1"),
    hh_size      = c(0),
    roster_count = c(3),
    stringsAsFactors = FALSE
  )

  res <- check_hh_composition(df, "hh_id", "hh_size",
                               roster_count_col = "roster_count")
  expect_equal(res$n_flagged, 1L)
  # Flag reason should mention both issues
  expect_true(grepl("implausible", res$flag_reason[1]))
  expect_true(grepl("roster", res$flag_reason[1], ignore.case = TRUE))
})

# -- check_income_expenditure --------------------------------------------------
test_that("check_income_expenditure flags excessive expenditure ratio", {
  df <- data.frame(
    hh_id       = paste0("H", 1:4),
    income      = c(1000, 500, 200, 1000),
    expenditure = c(2000, 400, 2000, 3000),
    stringsAsFactors = FALSE
  )

  res <- check_income_expenditure(df, "hh_id", "income", "expenditure",
                                   max_ratio = 5)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "G04_income_expenditure")
  expect_equal(res$check_category, "logic")
  # H3: 2000/200 = 10 > 5
  expect_true("H3" %in% res$flagged_ids)
  # H1: 2000/1000 = 2 <= 5 -> not flagged
  expect_false("H1" %in% res$flagged_ids)
})

test_that("check_income_expenditure skips zero income", {
  df <- data.frame(
    hh_id       = paste0("H", 1:2),
    income      = c(0, 100),
    expenditure = c(500, 50),
    stringsAsFactors = FALSE
  )

  res <- check_income_expenditure(df, "hh_id", "income", "expenditure",
                                   max_ratio = 5)
  # H1 has income=0, should be skipped (not flagged)
  expect_false("H1" %in% res$flagged_ids)
})

test_that("check_income_expenditure returns 0 for normal ratios", {
  df <- data.frame(
    hh_id       = paste0("H", 1:3),
    income      = c(1000, 2000, 3000),
    expenditure = c(800, 1500, 2000),
    stringsAsFactors = FALSE
  )

  res <- check_income_expenditure(df, "hh_id", "income", "expenditure",
                                   max_ratio = 5)
  expect_equal(res$n_flagged, 0L)
})

# -- check_age_date_consistency ------------------------------------------------
test_that("check_age_date_consistency flags age mismatch", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    age   = c(30, 25, 40),
    dob   = as.Date(c("1996-01-15", "2001-06-01", "1980-03-20")),
    stringsAsFactors = FALSE
  )

  # Use a fixed reference date for predictable results
  ref_date <- as.Date("2026-02-28")
  res <- check_age_date_consistency(df, "hh_id", "age", "dob",
                                     reference_date = ref_date,
                                     tolerance_years = 1)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F09_age_date_consistency")
  expect_equal(res$check_category, "logic")

  # H1: computed age ~30.1, reported 30 -> ok
  # H2: computed age ~24.7, reported 25 -> ok (within tolerance)
  # H3: computed age ~45.9, reported 40 -> mismatch ~5.9 years
  expect_true("H3" %in% res$flagged_ids)
})

test_that("check_age_date_consistency returns 0 for consistent data", {
  ref_date <- as.Date("2026-01-01")
  df <- data.frame(
    hh_id = paste0("H", 1:2),
    age   = c(30, 25),
    dob   = as.Date(c("1996-01-01", "2001-01-01")),
    stringsAsFactors = FALSE
  )

  res <- check_age_date_consistency(df, "hh_id", "age", "dob",
                                     reference_date = ref_date,
                                     tolerance_years = 1)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_age_date_consistency handles NA DOB", {
  df <- data.frame(
    hh_id = paste0("H", 1:2),
    age   = c(30, 25),
    dob   = as.Date(c("1996-01-01", NA)),
    stringsAsFactors = FALSE
  )

  res <- check_age_date_consistency(df, "hh_id", "age", "dob")
  # H2 has NA DOB, should be skipped (not flagged)
  expect_false("H2" %in% res$flagged_ids)
})

# -- check_survey_end_before_start ---------------------------------------------
test_that("check_survey_end_before_start flags end < start", {
  df <- data.frame(
    hh_id      = paste0("H", 1:3),
    start_time = as.character(as.POSIXct(c(
      "2026-01-15 10:00:00", "2026-01-15 14:00:00", "2026-01-15 09:00:00"
    ), tz = "UTC")),
    end_time   = as.character(as.POSIXct(c(
      "2026-01-15 11:00:00", "2026-01-15 13:00:00", "2026-01-15 10:00:00"
    ), tz = "UTC")),
    stringsAsFactors = FALSE
  )

  res <- check_survey_end_before_start(df, "hh_id", "start_time", "end_time")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F04_end_before_start")
  expect_equal(res$severity, "error")
  # H2: end 13:00 < start 14:00
  expect_true("H2" %in% res$flagged_ids)
  expect_false("H1" %in% res$flagged_ids)
  expect_false("H3" %in% res$flagged_ids)
})

test_that("check_survey_end_before_start returns 0 for valid times", {
  df <- data.frame(
    hh_id      = paste0("H", 1:2),
    start_time = c("2026-01-15 10:00:00", "2026-01-15 14:00:00"),
    end_time   = c("2026-01-15 11:00:00", "2026-01-15 15:00:00"),
    stringsAsFactors = FALSE
  )

  res <- check_survey_end_before_start(df, "hh_id", "start_time", "end_time")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_survey_end_before_start handles NA times", {
  df <- data.frame(
    hh_id      = paste0("H", 1:3),
    start_time = c("2026-01-15 10:00:00", NA, "2026-01-15 09:00:00"),
    end_time   = c("2026-01-15 11:00:00", "2026-01-15 12:00:00", NA),
    stringsAsFactors = FALSE
  )

  res <- check_survey_end_before_start(df, "hh_id", "start_time", "end_time")
  # NA times should be skipped, not flagged
  expect_equal(res$n_flagged, 0L)
})

# -- check_custom_logic -------------------------------------------------------
test_that("check_custom_logic flags records matching expression", {
  df <- data.frame(
    hh_id  = paste0("H", 1:4),
    age    = c(25, 15, 30, 10),
    income = c(100, 200, 0, 500),
    stringsAsFactors = FALSE
  )

  res <- check_custom_logic(df, "hh_id",
                             condition_expr = "age < 18 & income > 0",
                             description = "Underage with positive income")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "G12_custom_logic")
  expect_equal(res$check_category, "logic")
  # H2 (age=15, income=200) and H4 (age=10, income=500)
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_custom_logic returns 0 when no violations", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    age   = c(25, 30, 45),
    stringsAsFactors = FALSE
  )

  res <- check_custom_logic(df, "hh_id",
                             condition_expr = "age < 18",
                             description = "Underage respondent")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_custom_logic errors on non-logical expression", {
  df <- data.frame(hh_id = "H1", val = 10, stringsAsFactors = FALSE)

  expect_error(
    check_custom_logic(df, "hh_id", condition_expr = "val + 1"),
    "logical"
  )
})

# -- check_roster_completeness ------------------------------------------------
test_that("check_roster_completeness flags roster != hh_size", {
  df <- data.frame(
    hh_id        = paste0("H", 1:4),
    hh_size      = c(5, 4, 6, 3),
    member_count = c(5, 3, 6, 1),
    stringsAsFactors = FALSE
  )

  res <- check_roster_completeness(df, "hh_id", "hh_size", "member_count")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "C07_roster_completeness")
  expect_equal(res$check_category, "completeness")
  expect_equal(res$severity, "warning")
  # H2 (4 vs 3) and H4 (3 vs 1)
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_roster_completeness returns 0 when all match", {
  df <- data.frame(
    hh_id        = paste0("H", 1:3),
    hh_size      = c(5, 4, 6),
    member_count = c(5, 4, 6),
    stringsAsFactors = FALSE
  )

  res <- check_roster_completeness(df, "hh_id", "hh_size", "member_count")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_roster_completeness handles NA values", {
  df <- data.frame(
    hh_id        = paste0("H", 1:3),
    hh_size      = c(5, NA, 6),
    member_count = c(5, 3,  NA),
    stringsAsFactors = FALSE
  )

  res <- check_roster_completeness(df, "hh_id", "hh_size", "member_count")
  # H2 and H3 have NAs, only H1 is valid and matches
  expect_equal(res$n_flagged, 0L)
})

# -- check_duration_by_hh_size ------------------------------------------------
test_that("check_duration_by_hh_size flags too-short duration for large HH", {
  df <- data.frame(
    hh_id    = paste0("H", 1:4),
    duration = c(30, 5, 20, 60),
    hh_size  = c(5,  10, 3,  8),
    stringsAsFactors = FALSE
  )

  res <- check_duration_by_hh_size(df, "hh_id", "duration", "hh_size",
                                    min_minutes_per_member = 3)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "F06_duration_by_hh_size")
  expect_equal(res$check_category, "timing")
  expect_equal(res$severity, "warning")
  # H1: 30 >= 15 (5*3) -> ok
  # H2: 5 < 30 (10*3) -> flagged
  # H3: 20 >= 9 (3*3) -> ok
  # H4: 60 >= 24 (8*3) -> ok
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})

test_that("check_duration_by_hh_size returns 0 when all adequate", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    duration = c(30, 40, 50),
    hh_size  = c(3,  4,  5),
    stringsAsFactors = FALSE
  )

  res <- check_duration_by_hh_size(df, "hh_id", "duration", "hh_size",
                                    min_minutes_per_member = 3)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_duration_by_hh_size flag_reason includes expected minimum", {
  df <- data.frame(
    hh_id    = "H1",
    duration = 5,
    hh_size  = 10,
    stringsAsFactors = FALSE
  )

  res <- check_duration_by_hh_size(df, "hh_id", "duration", "hh_size",
                                    min_minutes_per_member = 3)
  expect_true(grepl("30", res$flag_reason[1]))  # 10 * 3 = 30
  expect_true(grepl("3", res$flag_reason[1]))
})
