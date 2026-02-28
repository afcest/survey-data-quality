# tests/testthat/test-check_corrections.R
# Tests for apply_corrections(), check_correction_log(), recode_other_specify()

# =============================================================================
# apply_corrections
# =============================================================================

test_that("apply_corrections applies valid corrections", {
  data <- data.frame(
    id   = c("1", "2", "3"),
    age  = c(25, 30, 40),
    name = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = c("1", "2"),
    variable  = c("age", "name"),
    old_value = c("25", "Bob"),
    new_value = c("26", "Robert"),
    stringsAsFactors = FALSE
  )

  result <- apply_corrections(data, log)

  expect_equal(result$n_applied, 2L)
  expect_equal(result$n_skipped, 0L)
  expect_equal(result$corrected_data$age[1], 26)
  expect_equal(result$corrected_data$name[2], "Robert")
  # Unchanged record
  expect_equal(result$corrected_data$age[3], 40)
})

test_that("apply_corrections skips when ID not found", {
  data <- data.frame(
    id  = c("1", "2"),
    age = c(25, 30),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = c("99"),
    variable  = c("age"),
    old_value = c("25"),
    new_value = c("26"),
    stringsAsFactors = FALSE
  )

  result <- apply_corrections(data, log)

  expect_equal(result$n_applied, 0L)
  expect_equal(result$n_skipped, 1L)
  expect_equal(nrow(result$skipped_log), 1)
  expect_true(grepl("ID not found", result$skipped_log$reason[1]))
})

test_that("apply_corrections skips when variable not found", {
  data <- data.frame(
    id  = c("1"),
    age = c(25),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = c("1"),
    variable  = c("nonexistent"),
    old_value = c("25"),
    new_value = c("26"),
    stringsAsFactors = FALSE
  )

  result <- apply_corrections(data, log)

  expect_equal(result$n_applied, 0L)
  expect_equal(result$n_skipped, 1L)
  expect_true(grepl("not found in data", result$skipped_log$reason[1]))
})

test_that("apply_corrections skips when old value does not match", {
  data <- data.frame(
    id  = c("1"),
    age = c(25),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = c("1"),
    variable  = c("age"),
    old_value = c("99"),
    new_value = c("26"),
    stringsAsFactors = FALSE
  )

  result <- apply_corrections(data, log)

  expect_equal(result$n_applied, 0L)
  expect_equal(result$n_skipped, 1L)
  expect_true(grepl("Old value mismatch", result$skipped_log$reason[1]))
})

test_that("apply_corrections handles empty correction log", {
  data <- data.frame(
    id  = c("1"),
    age = c(25),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = character(),
    variable  = character(),
    old_value = character(),
    new_value = character(),
    stringsAsFactors = FALSE
  )

  result <- apply_corrections(data, log)

  expect_equal(result$n_applied, 0L)
  expect_equal(result$n_skipped, 0L)
  expect_equal(nrow(result$skipped_log), 0)
  expect_equal(result$corrected_data$age, 25)
})

test_that("apply_corrections errors when required columns missing", {
  data <- data.frame(id = "1", age = 25, stringsAsFactors = FALSE)
  log <- data.frame(id = "1", stringsAsFactors = FALSE)

  expect_error(
    apply_corrections(data, log),
    "Missing required columns"
  )
})

# =============================================================================
# check_correction_log
# =============================================================================

test_that("check_correction_log returns 0 flags for clean log", {
  data <- data.frame(
    id  = c("1", "2"),
    age = c(25, 30),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    id        = c("1", "2"),
    variable  = c("age", "age"),
    old_value = c("25", "30"),
    stringsAsFactors = FALSE
  )

  res <- check_correction_log(data, log)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "Q02_correction_log")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$n_valid, 2L)
})

test_that("check_correction_log flags invalid ID", {
  data <- data.frame(id = "1", age = 25, stringsAsFactors = FALSE)
  log <- data.frame(
    id        = "99",
    variable  = "age",
    old_value = "25",
    stringsAsFactors = FALSE
  )

  res <- check_correction_log(data, log)

  expect_equal(res$n_flagged, 1L)
  expect_equal(res$summary_stat$n_invalid_id, 1L)
  expect_true(grepl("not found", res$flag_reason[1]))
})

test_that("check_correction_log flags invalid variable", {
  data <- data.frame(id = "1", age = 25, stringsAsFactors = FALSE)
  log <- data.frame(
    id        = "1",
    variable  = "nonexistent",
    old_value = "25",
    stringsAsFactors = FALSE
  )

  res <- check_correction_log(data, log)

  expect_equal(res$n_flagged, 1L)
  expect_equal(res$summary_stat$n_invalid_variable, 1L)
})

test_that("check_correction_log flags value mismatch", {
  data <- data.frame(id = "1", age = 25, stringsAsFactors = FALSE)
  log <- data.frame(
    id        = "1",
    variable  = "age",
    old_value = "99",
    stringsAsFactors = FALSE
  )

  res <- check_correction_log(data, log)

  expect_equal(res$n_flagged, 1L)
  expect_equal(res$summary_stat$n_value_mismatch, 1L)
})

test_that("check_correction_log handles empty log", {
  data <- data.frame(id = "1", age = 25, stringsAsFactors = FALSE)
  log <- data.frame(
    id        = character(),
    variable  = character(),
    old_value = character(),
    stringsAsFactors = FALSE
  )

  res <- check_correction_log(data, log)

  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

# =============================================================================
# recode_other_specify
# =============================================================================

test_that("recode_other_specify applies recode map correctly", {
  data <- data.frame(
    id    = c("1", "2", "3"),
    crop  = c("maiz", "riz paddy", "wheat"),
    stringsAsFactors = FALSE
  )
  recode_map <- c("maiz" = "maize", "riz paddy" = "rice")

  result <- recode_other_specify(data, "id", "crop", recode_map)

  expect_equal(result$crop[1], "maize")
  expect_equal(result$crop[2], "rice")
  # "wheat" not in map, stays as is
  expect_equal(result$crop[3], "wheat")
})

test_that("recode_other_specify is case insensitive", {
  data <- data.frame(
    id   = c("1", "2"),
    crop = c("MAIZ", "Maiz"),
    stringsAsFactors = FALSE
  )
  recode_map <- c("maiz" = "maize")

  result <- recode_other_specify(data, "id", "crop", recode_map)

  expect_equal(result$crop[1], "maize")
  expect_equal(result$crop[2], "maize")
})

test_that("recode_other_specify trims whitespace before matching", {
  data <- data.frame(
    id   = c("1"),
    crop = c("  maiz  "),
    stringsAsFactors = FALSE
  )
  recode_map <- c("maiz" = "maize")

  result <- recode_other_specify(data, "id", "crop", recode_map)

  expect_equal(result$crop[1], "maize")
})

test_that("recode_other_specify writes to target_col if specified", {
  data <- data.frame(
    id   = c("1", "2"),
    crop = c("maiz", "wheat"),
    stringsAsFactors = FALSE
  )
  recode_map <- c("maiz" = "maize")

  result <- recode_other_specify(data, "id", "crop", recode_map,
                                  target_col = "crop_recoded")

  # Original column unchanged
  expect_equal(result$crop[1], "maiz")
  # New column has recoded value
  expect_equal(result$crop_recoded[1], "maize")
  expect_equal(result$crop_recoded[2], "wheat")
})

test_that("recode_other_specify errors with invalid recode_map", {
  data <- data.frame(
    id   = c("1"),
    crop = c("maiz"),
    stringsAsFactors = FALSE
  )

  # recode_map must be a named character vector
  expect_error(
    recode_other_specify(data, "id", "crop", recode_map = c(1, 2))
  )

  # Empty names should error
  expect_error(
    recode_other_specify(data, "id", "crop", recode_map = c("maize"))
  )
})
