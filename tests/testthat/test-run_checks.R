# tests/testthat/test-run_checks.R
# Tests for run_all_checks(), print.adc_report(), summary.adc_report()

# =============================================================================
# Helper: create a minimal valid config
# =============================================================================
make_test_config <- function(checks = list()) {
  cfg <- list(
    project = list(
      name = "Test Survey"
    ),
    variables = list(
      id         = "id",
      enumerator = "enum"
    ),
    checks = checks
  )
  structure(cfg, class = "adc_config")
}

# =============================================================================
# run_all_checks
# =============================================================================

test_that("run_all_checks returns an adc_report object", {
  data <- data.frame(
    id   = c("1", "2", "3"),
    enum = c("E1", "E1", "E2"),
    stringsAsFactors = FALSE
  )
  config <- make_test_config(checks = list(
    identification = list(
      duplicate_ids = TRUE
    )
  ))

  report <- run_all_checks(data, config, verbose = FALSE)

  expect_s3_class(report, "adc_report")
  expect_true("results" %in% names(report))
  expect_true("summary" %in% names(report))
  expect_true("n_checks_run" %in% names(report))
  expect_true("n_checks_failed" %in% names(report))
  expect_true(inherits(report$timestamp, "POSIXct"))
})

test_that("run_all_checks respects disabled checks", {
  data <- data.frame(
    id   = c("1", "1", "2"),
    enum = c("E1", "E1", "E2"),
    stringsAsFactors = FALSE
  )
  config_disabled <- make_test_config(checks = list(
    identification = list(
      duplicate_ids = FALSE
    )
  ))

  report <- run_all_checks(data, config_disabled, verbose = FALSE)

  # duplicate_ids is disabled, so A01 should not appear
  check_names <- names(report$results)
  expect_false("A01_duplicate_id" %in% check_names)
})

test_that("run_all_checks handles empty data", {
  data <- data.frame(
    id   = character(),
    enum = character(),
    stringsAsFactors = FALSE
  )
  config <- make_test_config(checks = list(
    identification = list(
      duplicate_ids = TRUE
    )
  ))

  report <- run_all_checks(data, config, verbose = FALSE)

  expect_s3_class(report, "adc_report")
  # With 0 rows, checks may produce 0 flags
  expect_true(report$n_checks_run >= 0L)
})

test_that("run_all_checks accepts file path config", {
  skip_if_not_installed("yaml")

  # Create a temporary YAML config
  tmp_config <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp_config), add = TRUE)

  yaml_content <- list(
    project = list(name = "Temp Test"),
    variables = list(id = "id", enumerator = "enum"),
    checks = list(
      identification = list(duplicate_ids = TRUE)
    )
  )
  yaml::write_yaml(yaml_content, tmp_config)

  data <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    stringsAsFactors = FALSE
  )

  report <- run_all_checks(data, tmp_config, verbose = FALSE)

  expect_s3_class(report, "adc_report")
})

test_that("run_all_checks rejects invalid config", {
  data <- data.frame(id = "1", enum = "E1", stringsAsFactors = FALSE)

  expect_error(
    run_all_checks(data, list(not_a_config = TRUE), verbose = FALSE),
    "adc_config"
  )
})

# =============================================================================
# print.adc_report
# =============================================================================

test_that("print.adc_report prints without error", {
  data <- data.frame(
    id   = c("1", "1", "2"),
    enum = c("E1", "E1", "E2"),
    stringsAsFactors = FALSE
  )
  config <- make_test_config(checks = list(
    identification = list(duplicate_ids = TRUE)
  ))

  report <- run_all_checks(data, config, verbose = FALSE)

  # cli outputs to stderr/messages, so just verify no error
  expect_no_error(capture.output(print(report), type = "message"))
})

test_that("print.adc_report returns invisibly", {
  data <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    stringsAsFactors = FALSE
  )
  config <- make_test_config()

  report <- run_all_checks(data, config, verbose = FALSE)

  result <- withVisible(print(report))
  expect_false(result$visible)
})

# =============================================================================
# summary.adc_report
# =============================================================================

test_that("summary.adc_report returns a tibble", {
  data <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    stringsAsFactors = FALSE
  )
  config <- make_test_config(checks = list(
    identification = list(duplicate_ids = TRUE)
  ))

  report <- run_all_checks(data, config, verbose = FALSE)
  s <- summary(report)

  expect_s3_class(s, "tbl_df")
})

test_that("summary.adc_report includes expected columns", {
  data <- data.frame(
    id   = c("1", "2"),
    enum = c("E1", "E2"),
    stringsAsFactors = FALSE
  )
  config <- make_test_config(checks = list(
    identification = list(duplicate_ids = TRUE)
  ))

  report <- run_all_checks(data, config, verbose = FALSE)
  s <- summary(report)

  expected_cols <- c("check_name", "check_category", "severity",
                     "n_flagged", "n_total")
  for (col in expected_cols) {
    expect_true(col %in% names(s),
                info = paste("Missing column:", col))
  }
})
