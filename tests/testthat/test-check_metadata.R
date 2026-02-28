# tests/testthat/test-check_metadata.R
# Tests for metadata check functions (Phase 2)

# -- check_consent -------------------------------------------------------------
test_that("check_consent flags records without consent", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    consent = c(1, 0, 1, NA),
    stringsAsFactors = FALSE
  )

  res <- check_consent(df, "hh_id", "consent", consent_value = 1)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A06_consent")
  expect_equal(res$check_category, "metadata")
  expect_equal(res$severity, "error")
  # H2 (consent=0) and H4 (consent=NA) should be flagged
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_consent returns 0 when all consented", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    consent = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  res <- check_consent(df, "hh_id", "consent", consent_value = 1)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_consent handles string consent values", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    consent = c("yes", "no", "yes"),
    stringsAsFactors = FALSE
  )

  res <- check_consent(df, "hh_id", "consent", consent_value = "yes")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})

# -- check_form_version -------------------------------------------------------
test_that("check_form_version flags outdated versions", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    version = c("v3", "v2", "v3", "v1"),
    stringsAsFactors = FALSE
  )

  res <- check_form_version(df, "hh_id", "version", expected_version = "v3")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A07_form_version")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_form_version returns 0 when all versions match", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    version = c("v3", "v3", "v3"),
    stringsAsFactors = FALSE
  )

  res <- check_form_version(df, "hh_id", "version", expected_version = "v3")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_form_version works with numeric versions", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    version = c(3, 2, 3),
    stringsAsFactors = FALSE
  )

  res <- check_form_version(df, "hh_id", "version", expected_version = 3)
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
})

# -- check_interview_completed ------------------------------------------------
test_that("check_interview_completed flags incomplete surveys", {
  df <- data.frame(
    hh_id  = paste0("H", 1:4),
    status = c("complete", "partial", "complete", NA),
    stringsAsFactors = FALSE
  )

  res <- check_interview_completed(df, "hh_id", "status",
                                    complete_value = "complete")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A09_interview_completed")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_interview_completed returns 0 when all complete", {
  df <- data.frame(
    hh_id  = paste0("H", 1:3),
    status = c("complete", "complete", "complete"),
    stringsAsFactors = FALSE
  )

  res <- check_interview_completed(df, "hh_id", "status")
  expect_equal(res$n_flagged, 0L)
})

# -- check_survey_tracking ----------------------------------------------------
test_that("check_survey_tracking flags strata below target", {
  df <- data.frame(
    hh_id   = paste0("H", 1:7),
    stratum = c("A", "A", "A", "B", "B", "C", "C"),
    stringsAsFactors = FALSE
  )

  targets <- c(A = 5, B = 2, C = 3)
  res <- check_survey_tracking(df, "hh_id", "stratum",
                                target_per_stratum = targets)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A10_survey_tracking")
  expect_equal(res$severity, "warning")
  # A: 3/5 below, B: 2/2 ok, C: 2/3 below
  expect_true("A" %in% res$flagged_ids)
  expect_true("C" %in% res$flagged_ids)
  expect_false("B" %in% res$flagged_ids)
})

test_that("check_survey_tracking returns 0 when all strata meet targets", {
  df <- data.frame(
    hh_id   = paste0("H", 1:10),
    stratum = c(rep("A", 5), rep("B", 5)),
    stringsAsFactors = FALSE
  )

  targets <- c(A = 5, B = 5)
  res <- check_survey_tracking(df, "hh_id", "stratum",
                                target_per_stratum = targets)
  expect_equal(res$n_flagged, 0L)
})

# -- check_id_format ----------------------------------------------------------
test_that("check_id_format flags IDs not matching regex", {
  df <- data.frame(
    hh_id = c("HH-001", "HH-002", "XX-003", "123"),
    stringsAsFactors = FALSE
  )

  res <- check_id_format(df, "hh_id", pattern = "^HH-\\d{3}$")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A04_id_format")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 2L)
  expect_true("XX-003" %in% res$flagged_ids)
  expect_true("123" %in% res$flagged_ids)
})

test_that("check_id_format returns 0 when all IDs valid", {
  df <- data.frame(
    hh_id = c("HH-001", "HH-002", "HH-003"),
    stringsAsFactors = FALSE
  )

  res <- check_id_format(df, "hh_id", pattern = "^HH-\\d{3}$")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_id_format summary_stat tracks counts correctly", {
  df <- data.frame(
    hh_id = c("HH-001", "bad", NA),
    stringsAsFactors = FALSE
  )

  res <- check_id_format(df, "hh_id", pattern = "^HH-\\d{3}$")
  expect_equal(res$summary_stat$n_valid_format, 1L)
  expect_equal(res$summary_stat$n_bad_format, 1L)
  expect_equal(res$summary_stat$n_na, 1L)
})

# -- check_respondent_eligibility ----------------------------------------------
test_that("check_respondent_eligibility flags underage respondent", {
  df <- data.frame(
    hh_id = paste0("H", 1:4),
    age   = c(25, 17, 30, 15),
    stringsAsFactors = FALSE
  )

  res <- check_respondent_eligibility(df, "hh_id", age_col = "age", min_age = 18)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "A12_respondent_eligibility")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_respondent_eligibility returns 0 for all eligible", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    age   = c(25, 30, 45),
    stringsAsFactors = FALSE
  )

  res <- check_respondent_eligibility(df, "hh_id", age_col = "age", min_age = 18)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_respondent_eligibility flags ineligible gender", {
  df <- data.frame(
    hh_id  = paste0("H", 1:4),
    age    = c(25, 30, 25, 40),
    gender = c("F", "M", "F", "M"),
    stringsAsFactors = FALSE
  )

  res <- check_respondent_eligibility(
    df, "hh_id",
    age_col = "age", min_age = 18,
    gender_col = "gender", eligible_gender = "F"
  )
  # H2 and H4 are male, not in eligible_gender
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_respondent_eligibility flags both age and gender violations", {
  df <- data.frame(
    hh_id  = c("H1"),
    age    = c(15),
    gender = c("M"),
    stringsAsFactors = FALSE
  )

  res <- check_respondent_eligibility(
    df, "hh_id",
    age_col = "age", min_age = 18,
    gender_col = "gender", eligible_gender = "F"
  )
  expect_equal(res$n_flagged, 1L)
  # Flag reason should mention both violations
  expect_true(grepl("Age", res$flag_reason[1]))
  expect_true(grepl("Gender", res$flag_reason[1]))
})
