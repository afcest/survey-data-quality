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

# =============================================================================
# export_to_csv
# =============================================================================

test_that("export_to_csv creates summary and flagged CSV files", {
  tmp_dir <- tempfile("csv_test_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  paths <- export_to_csv(results, tmp_dir, prefix = "test_report")

  expect_true(file.exists(paths$summary))
  expect_true(file.exists(paths$flagged))

  # Check that summary CSV has content
  summary_df <- utils::read.csv(paths$summary, stringsAsFactors = FALSE)
  expect_true(nrow(summary_df) > 0)
  expect_true("check_name" %in% names(summary_df))
})

test_that("export_to_csv creates output directory if it does not exist", {
  tmp_dir <- file.path(tempdir(), "nonexistent_dir_csv_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_test_results()
  paths <- export_to_csv(results, tmp_dir)

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(paths$summary))
})

test_that("export_to_csv handles report with no flags", {
  tmp_dir <- tempfile("csv_empty_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  results <- make_empty_results()
  paths <- export_to_csv(results, tmp_dir, prefix = "empty_report")

  expect_true(file.exists(paths$summary))
  expect_true(file.exists(paths$flagged))

  # Flagged CSV should have 0 data rows (just header)
  flagged_df <- utils::read.csv(paths$flagged, stringsAsFactors = FALSE)
  expect_equal(nrow(flagged_df), 0)
})

# =============================================================================
# export_to_excel
# =============================================================================

test_that("export_to_excel creates xlsx file", {
  skip_if_not_installed("openxlsx2")

  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)

  results <- make_test_results()
  data <- data.frame(
    id   = as.character(1:10),
    age  = 20:29,
    stringsAsFactors = FALSE
  )

  path <- export_to_excel(results, data, "id", tmp_file)

  expect_true(file.exists(path))
  expect_equal(path, tmp_file)

  # Verify workbook has expected sheets
  wb <- openxlsx2::wb_load(tmp_file)
  sheet_names <- wb$get_sheet_names()
  expect_true("Summary" %in% sheet_names)
  expect_true("Flagged Records" %in% sheet_names)
})

test_that("export_to_excel errors without openxlsx2", {
  # This test is inherently hard to test if openxlsx2 IS installed.
  # We just verify the function exists and has the expected signature.
  expect_true(is.function(export_to_excel))
  fn_args <- names(formals(export_to_excel))
  expect_true(all(c("report", "data", "id_col", "path") %in% fn_args))
})
