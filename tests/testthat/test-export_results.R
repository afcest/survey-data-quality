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
# export_to_excel
# =============================================================================

test_that("export_to_excel creates xlsx file with IPA format sheets", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- make_test_data()

  path <- export_to_excel(results, data = data, id_col = "id", path = tmp_file,
                          enum_col = "enum_id", date_col = "sub_date")

  expect_true(file.exists(path))
  expect_equal(path, tmp_file)

  # Verify workbook has expected sheets
  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Dashboard" %in% sheet_names)
  expect_true("Flagged Records" %in% sheet_names)
  expect_true("Corrections" %in% sheet_names)
  expect_true("Enumerator Stats" %in% sheet_names)
  # Should have a category sheet for identification (has flags)
  expect_true("Identification" %in% sheet_names)
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
  expect_true("Dashboard" %in% sheet_names)
  expect_true("Flagged Records" %in% sheet_names)
  expect_true("Corrections" %in% sheet_names)
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
  expect_true("Dashboard" %in% sheet_names)
  expect_true("Flagged Records" %in% sheet_names)
})

test_that("export_to_excel has the expected function signature", {
  expect_true(is.function(export_to_excel))
  fn_args <- names(formals(export_to_excel))
  expect_true(all(c("report", "data", "id_col", "path",
                     "enum_col", "date_col", "keep_cols") %in% fn_args))
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
# keep_cols
# =============================================================================

test_that("export_to_excel includes keep_cols in Flagged Records", {
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
  flagged <- openxlsx2::wb_to_df(wb, sheet = "Flagged Records")
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
  flagged <- openxlsx2::wb_to_df(wb, sheet = "Flagged Records")
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
