# tests/testthat/test-check_backcheck.R
# Tests for check_backcheck_coverage(), check_backcheck_t1_match(),
# check_backcheck_t2_match(), check_backcheck_t3_match(),
# check_backcheck_by_enumerator()

# =============================================================================
# check_backcheck_t1_match
# =============================================================================

test_that("check_backcheck_t1_match detects mismatches on T1 variables", {
  orig <- data.frame(
    id     = c("1", "2", "3"),
    gender = c("M", "F", "M"),
    region = c("North", "South", "East"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id  = c("1", "2", "3"),
    gender = c("M", "M", "M"),
    region = c("North", "South", "East"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t1_match(orig, "id", bc, "bc_id",
                                   t1_cols = c("gender", "region"))

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "J02_backcheck_t1_match")
  expect_equal(res$severity, "error")
  # ID "2" has gender mismatch (F vs M)

expect_equal(res$n_flagged, 1L)
  expect_true("2" %in% res$flagged_ids)
  expect_equal(res$n_total, 3L)
})

test_that("check_backcheck_t1_match returns 0 flags when all match", {
  orig <- data.frame(
    id     = c("1", "2"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id  = c("1", "2"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t1_match(orig, "id", bc, "bc_id",
                                   t1_cols = "gender")

  expect_equal(res$n_flagged, 0L)
  expect_equal(length(res$flagged_ids), 0)
  expect_equal(res$summary_stat$overall_match_rate, 1)
})

test_that("check_backcheck_t1_match handles no matching IDs", {
  orig <- data.frame(
    id     = c("1", "2"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id  = c("99", "100"),
    gender = c("M", "F"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t1_match(orig, "id", bc, "bc_id",
                                   t1_cols = "gender")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
  expect_true(is.na(res$summary_stat$overall_match_rate))
})

test_that("check_backcheck_t1_match treats NA-NA as match", {
  orig <- data.frame(
    id     = c("1", "2"),
    gender = c(NA, "F"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id  = c("1", "2"),
    gender = c(NA, "F"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t1_match(orig, "id", bc, "bc_id",
                                   t1_cols = "gender")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$overall_match_rate, 1)
})

# =============================================================================
# check_backcheck_t2_match
# =============================================================================

test_that("check_backcheck_t2_match flags enumerator with high error rate", {
  orig <- data.frame(
    id   = c("1", "2", "3"),
    enum = c("E1", "E1", "E2"),
    q1   = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1", "2", "3"),
    q1    = c("X", "X", "C"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t2_match(orig, "id", bc, "bc_id",
                                   t2_cols = "q1",
                                   max_error_rate = 0.10,
                                   enum_col = "enum")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "J03_backcheck_t2_match")
  expect_equal(res$severity, "warning")
  # E1 has 100% error rate, should be flagged
  expect_true("E1" %in% res$flagged_ids)
  expect_equal(res$n_total, 3L)
})

test_that("check_backcheck_t2_match reports 0 flags with perfect match", {
  orig <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    q1   = c("A", "B"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1", "2"),
    q1    = c("A", "B"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t2_match(orig, "id", bc, "bc_id",
                                   t2_cols = "q1",
                                   enum_col = "enum")

  expect_equal(res$n_flagged, 0L)
})

test_that("check_backcheck_t2_match handles no matching IDs", {
  orig <- data.frame(
    id   = c("1"),
    enum = c("E1"),
    q1   = c("A"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("99"),
    q1    = c("A"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t2_match(orig, "id", bc, "bc_id",
                                   t2_cols = "q1",
                                   enum_col = "enum")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

test_that("check_backcheck_t2_match handles NA as mismatch with non-NA", {
  orig <- data.frame(
    id   = c("1"),
    enum = c("E1"),
    q1   = c("A"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1"),
    q1    = c(NA),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t2_match(orig, "id", bc, "bc_id",
                                   t2_cols = "q1",
                                   max_error_rate = 0.0,
                                   enum_col = "enum")

  # NA vs "A" is a mismatch
  expect_equal(res$n_flagged, 1L)
})

# =============================================================================
# check_backcheck_t3_match
# =============================================================================

test_that("check_backcheck_t3_match is always informational with 0 flags", {
  orig <- data.frame(
    id   = c("1", "2"),
    satisfaction = c("happy", "sad"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1", "2"),
    satisfaction = c("happy", "happy"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t3_match(orig, "id", bc, "bc_id",
                                   t3_cols = "satisfaction")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "J04_backcheck_t3_match")
  expect_equal(res$severity, "info")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 2L)
  # Change rate for satisfaction: 1 out of 2 changed
  expect_equal(res$summary_stat$change_rate_by_variable$n_changed, 1L)
})

test_that("check_backcheck_t3_match with no changes reports change_rate 0", {
  orig <- data.frame(
    id   = c("1", "2"),
    mood = c("ok", "good"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1", "2"),
    mood  = c("ok", "good"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_t3_match(orig, "id", bc, "bc_id",
                                   t3_cols = "mood")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$change_rate_by_variable$change_rate, 0)
})

test_that("check_backcheck_t3_match handles no matching IDs", {
  orig <- data.frame(id = "1", mood = "ok", stringsAsFactors = FALSE)
  bc <- data.frame(bc_id = "99", mood = "ok", stringsAsFactors = FALSE)

  res <- check_backcheck_t3_match(orig, "id", bc, "bc_id",
                                   t3_cols = "mood")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

test_that("check_backcheck_t3_match treats NA-NA as no change", {
  orig <- data.frame(id = "1", mood = NA_character_, stringsAsFactors = FALSE)
  bc <- data.frame(bc_id = "1", mood = NA_character_, stringsAsFactors = FALSE)

  res <- check_backcheck_t3_match(orig, "id", bc, "bc_id",
                                   t3_cols = "mood")

  expect_equal(res$summary_stat$change_rate_by_variable$n_changed, 0L)
})

# =============================================================================
# check_backcheck_coverage
# =============================================================================

test_that("check_backcheck_coverage flags low overall coverage", {
  orig <- data.frame(
    id = as.character(1:100),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = as.character(1:5),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_coverage(orig, "id", bc, "bc_id",
                                   min_coverage = 0.10)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "J01_backcheck_coverage")
  expect_equal(res$severity, "warning")
  # 5/100 = 5% < 10%, should flag
  expect_equal(res$n_flagged, 1L)
  expect_true(grepl("below minimum", res$flag_reason[1]))
})

test_that("check_backcheck_coverage passes when target met", {
  orig <- data.frame(
    id = as.character(1:10),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = as.character(1:2),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_coverage(orig, "id", bc, "bc_id",
                                   min_coverage = 0.10)

  # 2/10 = 20% >= 10%
  expect_equal(res$n_flagged, 0L)
})

test_that("check_backcheck_coverage computes per-stratum coverage", {
  orig <- data.frame(
    id     = as.character(1:20),
    region = rep(c("North", "South"), each = 10),
    stringsAsFactors = FALSE
  )
  # Only backcheck from North
  bc <- data.frame(
    bc_id = as.character(1:2),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_coverage(orig, "id", bc, "bc_id",
                                   strata_col = "region",
                                   min_coverage = 0.10)

  # North: 2/10 = 20% (ok), South: 0/10 = 0% (flagged)
  expect_equal(res$n_flagged, 1L)
  expect_true("South" %in% res$flagged_ids)
})

# =============================================================================
# check_backcheck_by_enumerator
# =============================================================================

test_that("check_backcheck_by_enumerator flags outlier enumerator", {
  # E1 has errors, E2-E5 have none => E1 should be outlier
  orig <- data.frame(
    id   = as.character(1:10),
    enum = rep(c("E1", "E2", "E3", "E4", "E5"), each = 2),
    q1   = c("wrong1", "wrong2", "A", "B", "C", "D", "E", "F", "G", "H"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = as.character(1:10),
    q1    = c("A", "B", "A", "B", "C", "D", "E", "F", "G", "H"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_by_enumerator(orig, "id", "enum", bc, "bc_id",
                                        compare_cols = "q1",
                                        threshold_sd = 1.5)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "J06_backcheck_by_enumerator")
  expect_equal(res$severity, "warning")
  # E1 should be flagged as outlier
  expect_true("E1" %in% res$flagged_ids)
})

test_that("check_backcheck_by_enumerator returns 0 flags when all uniform", {
  orig <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    q1   = c("A", "B"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("1", "2"),
    q1    = c("A", "B"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_by_enumerator(orig, "id", "enum", bc, "bc_id",
                                        compare_cols = "q1")

  # sd = 0, so all z_scores = 0 => no flags
  expect_equal(res$n_flagged, 0L)
})

test_that("check_backcheck_by_enumerator handles no matches", {
  orig <- data.frame(
    id   = c("1"),
    enum = c("E1"),
    q1   = c("A"),
    stringsAsFactors = FALSE
  )
  bc <- data.frame(
    bc_id = c("99"),
    q1    = c("A"),
    stringsAsFactors = FALSE
  )

  res <- check_backcheck_by_enumerator(orig, "id", "enum", bc, "bc_id",
                                        compare_cols = "q1")

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
  expect_true(is.na(res$summary_stat$team_mean))
})
