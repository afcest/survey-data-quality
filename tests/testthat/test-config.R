# tests/testthat/test-config.R
# Tests for read_config(), validate_config(), cfg_get()

test_that("read_config reads a valid YAML config", {
  cfg_content <- '
project:
  name: "Test Survey"
  survey_start: "2025-01-01"
  survey_end: "2025-06-30"
variables:
  id: "hh_id"
  enumerator: "enum_id"
checks:
  identification:
    duplicate_ids: true
'
  tmp <- tempfile(fileext = ".yml")
  writeLines(cfg_content, tmp)
  on.exit(unlink(tmp))

  cfg <- read_config(tmp)
  expect_s3_class(cfg, "adc_config")
  expect_equal(cfg$project$name, "Test Survey")
  expect_equal(cfg$variables$id, "hh_id")
  expect_equal(cfg$variables$enumerator, "enum_id")
})

test_that("read_config errors on non-existent file", {
  expect_error(read_config("/no/such/file.yml"), "not found")
})

test_that("validate_config catches missing 'project' section", {
  cfg <- list(variables = list(id = "x", enumerator = "y"))
  expect_error(validate_config(cfg), "project")
})

test_that("validate_config catches missing 'variables' section", {
  cfg <- list(project = list(name = "Test"))
  expect_error(validate_config(cfg), "variables")
})

test_that("validate_config catches missing required variable fields", {
  cfg <- list(
    project   = list(name = "Test"),
    variables = list(date = "col_date")
  )
  expect_error(validate_config(cfg), "id")
})

test_that("validate_config catches missing enumerator field", {
  cfg <- list(
    project   = list(name = "Test"),
    variables = list(id = "hh_id")
  )
  expect_error(validate_config(cfg), "enumerator")
})

test_that("validate_config passes with all required sections and fields", {
  cfg <- list(
    project   = list(name = "Test"),
    variables = list(id = "hh_id", enumerator = "enum_id")
  )
  result <- validate_config(cfg)
  expect_type(result, "list")
})

test_that("cfg_get retrieves top-level values", {
  cfg_content <- '
project:
  name: "Test"
variables:
  id: "hh_id"
  enumerator: "enum_id"
'
  tmp <- tempfile(fileext = ".yml")
  writeLines(cfg_content, tmp)
  on.exit(unlink(tmp))
  cfg <- read_config(tmp)

  expect_equal(cfg_get(cfg, "project", "name"), "Test")
})

test_that("cfg_get retrieves deeply nested values", {
  cfg_content <- '
project:
  name: "Nested Test"
variables:
  id: "hh_id"
  enumerator: "enum_id"
checks:
  outliers:
    method: "iqr"
    multiplier: 1.5
'
  tmp <- tempfile(fileext = ".yml")
  writeLines(cfg_content, tmp)
  on.exit(unlink(tmp))
  cfg <- read_config(tmp)

  expect_equal(cfg_get(cfg, "checks", "outliers", "method"), "iqr")
  expect_equal(cfg_get(cfg, "checks", "outliers", "multiplier"), 1.5)
})

test_that("cfg_get returns default for missing paths", {
  cfg_content <- '
project:
  name: "Default Test"
variables:
  id: "hh_id"
  enumerator: "enum_id"
'
  tmp <- tempfile(fileext = ".yml")
  writeLines(cfg_content, tmp)
  on.exit(unlink(tmp))
  cfg <- read_config(tmp)

  expect_null(cfg_get(cfg, "nonexistent", "path"))
  expect_equal(cfg_get(cfg, "nonexistent", "path", default = "fallback"), "fallback")
  expect_equal(cfg_get(cfg, "checks", "outliers", "method", default = "zscore"), "zscore")
})

test_that("cfg_get returns NULL by default for missing keys", {
  cfg_content <- '
project:
  name: "Null Test"
variables:
  id: "hh_id"
  enumerator: "enum_id"
'
  tmp <- tempfile(fileext = ".yml")
  writeLines(cfg_content, tmp)
  on.exit(unlink(tmp))
  cfg <- read_config(tmp)

  expect_null(cfg_get(cfg, "does_not_exist"))
})
