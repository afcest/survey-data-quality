# tests/testthat/test-check_paradata.R
# Tests for check_text_audit_duration(), check_text_audit_sequence(),
# check_speed_violations(), check_field_comments(),
# check_photo_attachment(), check_device_consistency()

# =============================================================================
# check_text_audit_duration
# =============================================================================

test_that("check_text_audit_duration flags fast questions", {
  data <- data.frame(
    id       = c("S1", "S1", "S2", "S2"),
    question = c("q1", "q2", "q1", "q2"),
    duration = c(1, 5, 2, 10),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_duration(data, "id", "question", "duration",
                                    min_seconds = 3)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N01_text_audit_duration")
  expect_equal(res$severity, "warning")
  # S1 has q1=1s (too fast), S2 has q1=2s (too fast)
  expect_equal(res$n_flagged, 2L)
  expect_true("S1" %in% res$flagged_ids)
  expect_true("S2" %in% res$flagged_ids)
})

test_that("check_text_audit_duration returns 0 flags when all adequate", {
  data <- data.frame(
    id       = c("S1", "S1"),
    question = c("q1", "q2"),
    duration = c(5, 10),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_duration(data, "id", "question", "duration",
                                    min_seconds = 3)

  expect_equal(res$n_flagged, 0L)
})

test_that("check_text_audit_duration excludes NA durations", {
  data <- data.frame(
    id       = c("S1", "S1"),
    question = c("q1", "q2"),
    duration = c(NA, 5),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_duration(data, "id", "question", "duration",
                                    min_seconds = 3)

  # NA is excluded, only q2 is checked (5s >= 3s, ok)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_text_audit_duration summary includes violations by question", {
  data <- data.frame(
    id       = c("S1", "S2", "S3"),
    question = c("q1", "q1", "q2"),
    duration = c(1, 2, 1),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_duration(data, "id", "question", "duration",
                                    min_seconds = 3)

  expect_true("violations_by_question" %in% names(res$summary_stat))
  vbq <- res$summary_stat$violations_by_question
  # q1 has 2 violations, q2 has 1
  expect_true("q1" %in% vbq$question)
})

# =============================================================================
# check_text_audit_sequence
# =============================================================================

test_that("check_text_audit_sequence detects backward jumps", {
  data <- data.frame(
    id       = c("S1", "S1", "S1", "S2", "S2", "S2"),
    question = c("q1", "q2", "q3", "q1", "q2", "q3"),
    seq_order = c(1, 3, 2, 1, 2, 3),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_sequence(data, "id", "question", "seq_order")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N02_text_audit_sequence")
  expect_equal(res$severity, "info")
  # S1 has backward jump (3 -> 2), S2 is monotonic
  expect_equal(res$n_flagged, 1L)
  expect_true("S1" %in% res$flagged_ids)
})

test_that("check_text_audit_sequence returns 0 for monotonic sequences", {
  data <- data.frame(
    id        = c("S1", "S1", "S1"),
    question  = c("q1", "q2", "q3"),
    seq_order = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  res <- check_text_audit_sequence(data, "id", "question", "seq_order")

  expect_equal(res$n_flagged, 0L)
})

# =============================================================================
# check_speed_violations
# =============================================================================

test_that("check_speed_violations flags fast surveys", {
  data <- data.frame(
    id             = c("S1", "S2", "S3"),
    total_duration = c(100, 500, 600),
    stringsAsFactors = FALSE
  )

  # 20 questions * 5s = 100s minimum => S1 is exactly at min, not flagged
  # But let's use total_questions=20, min_seconds_per_question=5 => 100
  res <- check_speed_violations(data, "id",
                                 total_questions = 20,
                                 total_duration_col = "total_duration",
                                 min_seconds_per_question = 10)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N03_speed_violations")
  expect_equal(res$severity, "warning")
  # Expected min: 20*10 = 200s. S1=100 (<200, flagged)
  expect_equal(res$n_flagged, 1L)
  expect_true("S1" %in% res$flagged_ids)
})

test_that("check_speed_violations returns 0 flags when all adequate", {
  data <- data.frame(
    id             = c("S1", "S2"),
    total_duration = c(500, 600),
    stringsAsFactors = FALSE
  )

  res <- check_speed_violations(data, "id",
                                 total_questions = 10,
                                 total_duration_col = "total_duration",
                                 min_seconds_per_question = 5)

  # min = 50s. Both above.
  expect_equal(res$n_flagged, 0L)
})

test_that("check_speed_violations excludes NA durations", {
  data <- data.frame(
    id             = c("S1", "S2"),
    total_duration = c(NA, 500),
    stringsAsFactors = FALSE
  )

  res <- check_speed_violations(data, "id",
                                 total_questions = 10,
                                 total_duration_col = "total_duration",
                                 min_seconds_per_question = 5)

  # NA excluded, S2 is fine
  expect_equal(res$n_flagged, 0L)
})

# =============================================================================
# check_field_comments
# =============================================================================

test_that("check_field_comments flags non-empty comments", {
  data <- data.frame(
    id      = c("1", "2", "3"),
    comment = c("Please review", "", NA),
    stringsAsFactors = FALSE
  )

  res <- check_field_comments(data, "id", "comment")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N04_field_comments")
  expect_equal(res$severity, "info")
  # Only "1" has a non-empty comment
  expect_equal(res$n_flagged, 1L)
  expect_true("1" %in% res$flagged_ids)
})

test_that("check_field_comments returns 0 when all empty", {
  data <- data.frame(
    id      = c("1", "2"),
    comment = c("", NA),
    stringsAsFactors = FALSE
  )

  res <- check_field_comments(data, "id", "comment")

  expect_equal(res$n_flagged, 0L)
})

# =============================================================================
# check_photo_attachment
# =============================================================================

test_that("check_photo_attachment flags missing photos", {
  data <- data.frame(
    id    = c("1", "2", "3", "4"),
    photo = c("photo1.jpg", NA, "", "none"),
    stringsAsFactors = FALSE
  )

  res <- check_photo_attachment(data, "id", "photo")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N06_photo_attachment")
  expect_equal(res$severity, "warning")
  # IDs 2 (NA), 3 (empty), 4 ("none") are flagged
  expect_equal(res$n_flagged, 3L)
  expect_false("1" %in% res$flagged_ids)
  expect_true(all(c("2", "3", "4") %in% res$flagged_ids))
})

test_that("check_photo_attachment returns 0 when all present", {
  data <- data.frame(
    id    = c("1", "2"),
    photo = c("a.jpg", "b.png"),
    stringsAsFactors = FALSE
  )

  res <- check_photo_attachment(data, "id", "photo")

  expect_equal(res$n_flagged, 0L)
})

# =============================================================================
# check_device_consistency
# =============================================================================

test_that("check_device_consistency flags multi-device enumerators", {
  data <- data.frame(
    id     = c("1", "2", "3", "4"),
    enum   = c("E1", "E1", "E2", "E2"),
    device = c("D1", "D2", "D3", "D3"),
    stringsAsFactors = FALSE
  )

  res <- check_device_consistency(data, "id", "enum", "device")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "N08_device_consistency")
  expect_equal(res$severity, "warning")
  # E1 used 2 devices (D1, D2) => flagged; E2 used 1 device => ok
  expect_true(res$n_flagged > 0L)
  # Flagged IDs are the survey IDs of E1
  expect_true(all(c("1", "2") %in% res$flagged_ids))
})

test_that("check_device_consistency returns 0 for single device per enum", {
  data <- data.frame(
    id     = c("1", "2", "3"),
    enum   = c("E1", "E1", "E2"),
    device = c("D1", "D1", "D2"),
    stringsAsFactors = FALSE
  )

  res <- check_device_consistency(data, "id", "enum", "device")

  expect_equal(res$n_flagged, 0L)
})

test_that("check_device_consistency excludes NA enum/device", {
  data <- data.frame(
    id     = c("1", "2", "3"),
    enum   = c("E1", NA, "E1"),
    device = c("D1", "D1", NA),
    stringsAsFactors = FALSE
  )

  res <- check_device_consistency(data, "id", "enum", "device")

  # Only row 1 is valid (E1, D1). Row 2 has NA enum, row 3 has NA device.
  # E1 used only D1 => no flag
  expect_equal(res$n_flagged, 0L)
})
