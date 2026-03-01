# tests/testthat/test-export_results.R
# Tests for export_to_csv(), export_to_excel()

# =============================================================================
# Helper: create a list of check_result objects for testing
# =============================================================================
make_test_results <- function() {
  list(
    new_check_result(
      check_name     = "A01_duplicate_id",
      check_category = "identification",
      n_flagged      = 2L,
      n_total        = 10L,
      flagged_ids    = c("1", "2"),
      flag_reason    = c("Duplicate ID: 1", "Duplicate ID: 2"),
      severity       = "error"
    ),
    new_check_result(
      check_name     = "F01_survey_duration",
      check_category = "timing",
      n_flagged      = 0L,
      n_total        = 10L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning"
    )
  )
}

make_empty_results <- function() {
  list(
    new_check_result(
      check_name     = "A01_duplicate_id",
      check_category = "identification",
      n_flagged      = 0L,
      n_total        = 5L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "error"
    )
  )
}

make_test_data <- function() {
  data.frame(
    id        = as.character(1:10),
    enum_id   = rep(c("E01", "E02"), each = 5),
    sub_date  = rep("2025-01-20", 10),
    age       = 20:29,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# export_to_csv
# =============================================================================

test_that("export_to_csv creates dashboard and flagged_records CSV files", {
  tmp_dir <- tempfile("csv_test_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  paths <- export_to_csv(results, tmp_dir, prefix = "test_report")

  expect_true(file.exists(paths$dashboard))
  expect_true(file.exists(paths$flagged_records))
  expect_true(file.exists(paths$corrections))

  # Check that dashboard CSV has content
  dashboard_df <- utils::read.csv(paths$dashboard, stringsAsFactors = FALSE)
  expect_true(nrow(dashboard_df) > 0)
  expect_true("Description" %in% names(dashboard_df))
})

test_that("export_to_csv creates output directory if it does not exist", {
  tmp_dir <- file.path(tempdir(), "nonexistent_dir_csv_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  paths <- export_to_csv(results, tmp_dir)

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(paths$dashboard))
})

test_that("export_to_csv handles report with no flags", {
  tmp_dir <- tempfile("csv_empty_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_empty_results()
  paths <- export_to_csv(results, tmp_dir, prefix = "empty_report")

  expect_true(file.exists(paths$dashboard))
  expect_true(file.exists(paths$flagged_records))

  # Flagged CSV should have 0 data rows (just header)
  flagged_df <- utils::read.csv(paths$flagged_records, stringsAsFactors = FALSE)
  expect_equal(nrow(flagged_df), 0)
})

test_that("export_to_csv includes enumerator stats when enum_col provided", {
  tmp_dir <- tempfile("csv_enum_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  test_data <- make_test_data()
  paths <- export_to_csv(results, tmp_dir, prefix = "enum_test",
                         data = test_data, id_col = "id",
                         enum_col = "enum_id", date_col = "sub_date")

  expect_true(file.exists(paths$enumerator_stats))
  enum_df <- utils::read.csv(paths$enumerator_stats, stringsAsFactors = FALSE)
  expect_true("enumerator" %in% names(enum_df))
  expect_true("flag_rate" %in% names(enum_df))
})

# =============================================================================
# export_to_excel — AfCEST multi-sheet format
# =============================================================================

test_that("export_to_excel creates xlsx file with AfCEST format sheets", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id", path = tmp_file,
                          enum_col = "enum_id", date_col = "sub_date")

  expect_true(file.exists(path))
  expect_equal(path, tmp_file)

  # Verify workbook has expected AfCEST-format sheets
  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  expect_true("All Flags" %in% sheet_names)
  expect_true("Corrections Log" %in% sheet_names)
  expect_true("Enumerator Performance" %in% sheet_names)
  # Should have duplicate IDs sheet (identification has flags)
  expect_true("Duplicate IDs" %in% sheet_names)
})

test_that("export_to_excel handles empty report gracefully", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_empty_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id", path = tmp_file)

  expect_true(file.exists(path))
  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  expect_true("All Flags" %in% sheet_names)
  expect_true("Corrections Log" %in% sheet_names)
})

test_that("export_to_excel works without optional params", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()

  # No data, no enum_col, no date_col
  path <- export_to_excel(results, path = tmp_file)

  expect_true(file.exists(path))
  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  expect_true("All Flags" %in% sheet_names)
})

test_that("export_to_excel has the expected function signature", {
  expect_true(is.function(export_to_excel))
  fn_args <- names(formals(export_to_excel))
  expect_true(all(c("report", "data", "id_col", "path",
                     "enum_col", "date_col", "keep_cols",
                     "sheets", "backcheck_data") %in% fn_args))
})

test_that("export_to_excel works with adc_report objects", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  # Build a minimal adc_report
  results_list <- make_test_results()
  report <- structure(
    list(
      results        = results_list,
      summary        = bind_check_results(results_list),
      config         = NULL,
      n_checks_run   = 2L,
      n_checks_failed = 1L,
      errors         = list(),
      timestamp      = Sys.time()
    ),
    class = "adc_report"
  )

  path <- export_to_excel(report, path = tmp_file)
  expect_true(file.exists(path))
})

# =============================================================================
# sheets parameter
# =============================================================================

test_that("sheets = 'all' produces all available sheets", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id",
                          path = tmp_file, enum_col = "enum_id",
                          date_col = "sub_date", sheets = "all")

  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  # Must have Guide, Check Summary, All Flags, Corrections Log at minimum
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  expect_true("All Flags" %in% sheet_names)
  expect_true("Corrections Log" %in% sheet_names)
})

test_that("sheets parameter selects only specified sheets plus README", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id",
                          path = tmp_file,
                          sheets = c("summary", "corrections"))

  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  # Should have Guide (always included) + Check Summary + Corrections Log
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  expect_true("Corrections Log" %in% sheet_names)
  # Should NOT have other sheets
  expect_false("All Flags" %in% sheet_names)
  expect_false("Duplicate IDs" %in% sheet_names)
})

test_that("negative sheets parameter excludes specified sheets", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id",
                          path = tmp_file, enum_col = "enum_id",
                          date_col = "sub_date",
                          sheets = c("-fabrication", "-back_check"))

  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  # Should have core sheets
  expect_true("Guide" %in% sheet_names)
  expect_true("Check Summary" %in% sheet_names)
  # Fabrication Flags and Back-Check Results should not be present since excluded
  expect_false("Fabrication Flags" %in% sheet_names)
  expect_false("Back-Check Results" %in% sheet_names)
})

# =============================================================================
# Sheet content validation
# =============================================================================

test_that("Summary sheet has PASS/FAIL status column", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  path <- export_to_excel(results, path = tmp_file,
                          sheets = c("summary"))

  wb <- openxlsx2::wb_load(tmp_file)
  summary_df <- openxlsx2::wb_to_df(wb, sheet = "Check Summary")
  expect_true("status" %in% names(summary_df))
  # A01_duplicate_id has flags -> FAIL, F01_survey_duration has 0 -> PASS
  expect_true("FAIL" %in% summary_df$status)
  expect_true("PASS" %in% summary_df$status)
})

# =============================================================================
# keep_cols
# =============================================================================

test_that("export_to_excel includes keep_cols in All Flags", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  test_data <- make_test_data()
  test_data$village <- paste0("Village_", seq_len(nrow(test_data)))
  test_data$phone   <- paste0("077000000", seq_len(nrow(test_data)))

  path <- export_to_excel(results, data = test_data, id_col = "id",
                          path = tmp_file, enum_col = "enum_id",
                          date_col = "sub_date",
                          keep_cols = c("village", "phone"))

  wb <- openxlsx2::wb_load(tmp_file)
  flagged <- openxlsx2::wb_to_df(wb, sheet = "All Flags")
  expect_true("village" %in% names(flagged))
  expect_true("phone" %in% names(flagged))
  # Values should be populated for flagged IDs
  expect_true(all(!is.na(flagged$village)))
  expect_true(all(!is.na(flagged$phone)))
})

test_that("keep_cols warns about missing columns and skips them", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  test_data <- make_test_data()
  test_data$village <- paste0("V", seq_len(nrow(test_data)))

  expect_warning(
    export_to_excel(results, data = test_data, id_col = "id",
                    path = tmp_file,
                    keep_cols = c("village", "nonexistent_col")),
    "nonexistent_col"
  )

  wb <- openxlsx2::wb_load(tmp_file)
  flagged <- openxlsx2::wb_to_df(wb, sheet = "All Flags")
  expect_true("village" %in% names(flagged))
  expect_false("nonexistent_col" %in% names(flagged))
})

test_that("export_to_csv includes keep_cols in flagged records", {
  tmp_dir <- tempfile("csv_keep_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  test_data <- make_test_data()
  test_data$village <- paste0("V", seq_len(nrow(test_data)))

  paths <- export_to_csv(results, tmp_dir, prefix = "keep_test",
                         data = test_data, id_col = "id",
                         keep_cols = c("village"))

  flagged_df <- utils::read.csv(paths$flagged_records, stringsAsFactors = FALSE)
  expect_true("village" %in% names(flagged_df))
})

test_that("keep_cols columns appear in empty flagged table", {
  tmp_dir <- tempfile("csv_keep_empty_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_empty_results()
  test_data <- make_test_data()
  test_data$village <- paste0("V", seq_len(nrow(test_data)))

  paths <- export_to_csv(results, tmp_dir, prefix = "keep_empty",
                         data = test_data, id_col = "id",
                         keep_cols = c("village"))

  flagged_df <- utils::read.csv(paths$flagged_records, stringsAsFactors = FALSE)
  expect_true("village" %in% names(flagged_df))
  expect_equal(nrow(flagged_df), 0)
})

# =============================================================================
# All builders produce output
# =============================================================================

test_that("all builders produce output even with empty results", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_empty_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id",
                          path = tmp_file, enum_col = "enum_id",
                          date_col = "sub_date", sheets = "all")

  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  # All 18 sheets should be present
  expect_true(length(sheet_names) >= 18)
})

# =============================================================================
# Edge case tests from review
# =============================================================================

test_that("sheets = NA produces a clear error", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  expect_error(
    export_to_excel(results, path = tmp_file, sheets = NA),
    "must not contain NA"
  )
})

test_that("sheets = character(0) produces a clear error", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  expect_error(
    export_to_excel(results, path = tmp_file, sheets = character(0)),
    "must not be empty"
  )
})

test_that("mixed positive/negative sheets produces a clear error", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  expect_error(
    export_to_excel(results, path = tmp_file,
                    sheets = c("summary", "-fabrication")),
    "all positive or all negative"
  )
})

test_that("keep_cols with reserved column names are silently excluded", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  test_data <- make_test_data()
  # Add columns that collide with reserved names
  test_data$severity <- rep("custom", 10)
  test_data$village <- paste0("V", seq_len(10))

  path <- export_to_excel(results, data = test_data, id_col = "id",
                          path = tmp_file,
                          keep_cols = c("severity", "village"))

  wb <- openxlsx2::wb_load(tmp_file)
  flagged <- openxlsx2::wb_to_df(wb, sheet = "All Flags")
  # village should be present, but severity should be the check severity not data column
  expect_true("village" %in% names(flagged))
  # The severity column should contain "error" (from check), not "custom" (from data)
  expect_true(any(flagged$severity == "error"))
  expect_false(any(flagged$severity == "custom", na.rm = TRUE))
})

test_that("single enumerator data works for enumerator dashboard", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  # Create data with single enumerator
  single_enum_data <- data.frame(
    id = as.character(1:5),
    enum_id = rep("E01", 5),
    sub_date = rep("2025-01-20", 5),
    duration = c(30, 45, 20, 60, 35),
    stringsAsFactors = FALSE
  )

  results <- list(
    new_check_result(
      check_name = "A01_duplicate_id",
      check_category = "identification",
      n_flagged = 1L, n_total = 5L,
      flagged_ids = "1",
      flag_reason = "Duplicate ID: 1",
      severity = "error"
    )
  )

  path <- export_to_excel(results, data = single_enum_data, id_col = "id",
                          path = tmp_file, enum_col = "enum_id",
                          date_col = "sub_date",
                          sheets = c("enumerator_dashboard"))

  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Enumerator Performance" %in% sheet_names)

  enum_df <- openxlsx2::wb_to_df(wb, sheet = "Enumerator Performance")
  # Should NOT be a "No issues" message - should have actual stats
  expect_false("message" %in% names(enum_df))
  expect_true("enumerator" %in% names(enum_df))
  expect_equal(nrow(enum_df), 1)
  expect_true("avg_duration" %in% names(enum_df))
})

test_that("export_to_csv works when flagged table building fails", {
  tmp_dir <- tempfile("csv_error_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Use a malformed result - n_flagged doesn't match flagged_ids
  results <- list(
    structure(
      list(
        check_name = "test_check",
        check_category = "identification",
        n_flagged = 5L,
        n_total = 10L,
        flagged_ids = character(0),
        flag_reason = character(0),
        severity = "error",
        summary_stat = list()
      ),
      class = "check_result"
    )
  )

  # Should not crash - should warn and continue
  expect_warning(
    paths <- export_to_csv(results, tmp_dir, prefix = "error_test"),
    "Failed to build flagged table"
  )
  expect_true(file.exists(paths$dashboard))
})
