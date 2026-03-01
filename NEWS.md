# afcestDataCheck 0.3.0

## New features

* **Back-check reconciliation** (5 functions): `check_backcheck_t1_match()`,
  `check_backcheck_t2_match()`, `check_backcheck_t3_match()`,
  `check_backcheck_coverage()`, `check_backcheck_by_enumerator()`.
  Three-tier back-check system (T1: exact match for immutable variables,
  T2: categorical agreement rates, T3: continuous tolerance).

* **Paradata checks** (6 functions): `check_text_audit_duration()`,
  `check_text_audit_sequence()`, `check_speed_violations()`,
  `check_field_comments()`, `check_photo_attachment()`,
  `check_device_consistency()`.

* **Correction workflow** (3 functions): `check_correction_log()` validates
  a correction log before applying; `apply_corrections()` applies corrections
  with old-value verification; `recode_other_specify()` recodes free-text
  "other (specify)" responses into existing categories.

* **Bundled sample datasets**: `survey_sample` (100 rows, 22 columns with 15+
  planted quality issues), `backcheck_sample` (15 back-check interviews),
  `corrections_sample` (5-entry correction log). All created via
  `data-raw/survey_sample.R`.

* **Comprehensive reference vignette** (`afcestDataCheck-reference.Rmd`):
  2500+ line PDF manual covering all 65+ check functions with signatures,
  examples, and complete function index.

* **Getting-started vignette** (`getting-started.Rmd`): Step-by-step guide
  with sample data, individual checks, orchestrator, export, and Shiny
  integration examples.

## Improvements

* `export_to_excel()` now produces 5-sheet IPA-style workbooks: Dashboard,
  Flagged Records (severity color-coded), per-category sheets, Corrections
  template (with action dropdown), and Enumerator Stats.

* `export_to_csv()` writes 4 CSV files: dashboard, flagged records,
  corrections template, and enumerator stats.

* `run_all_checks()` orchestrator now supports `backcheck_data` and
  `sampling_frame` parameters.

* All check functions now include a `timestamp` field in the returned
  `check_result` object.

## Bug fixes

* Fixed `export_to_excel()` variable/value columns being NA across most sheets.
  `.extract_variable_from_check()` heuristic replaced with `.get_check_variable()`
  that reads `summary_stat$variable` first (where outlier, range, fabrication
  checks store it).
* Fixed Missing Data sheet: now extracts per-variable stats from C01's
  `miss_stats` tibble and C04's `flagged_ids` instead of producing a single
  broken row.
* Fixed Fabrication Flags sheet: one clean row per variable with `check_type`,
  `test_statistic`, `p_value`, and `result` columns instead of exploded
  `stat_*` columns from nested `summary_stat`.
* Fixed Duplicate IDs sheet: now shows only key identifying columns (ID,
  enumerator, date, name, phone, village, district) instead of all 31+ data
  columns.
* Fixed Corrections Log: filters to record-level actionable flags only
  (excludes fabrication and aggregate completeness checks).
* Added human-readable `description` column to Check Summary sheet with
  plain-English explanations for 30+ check codes.
* Fixed `check_dk_rate()` vectorization bug when multiple DK values provided.
* Fixed `check_duplicate_ids()` dead code path and O(n^2) performance issue.
* Fixed `normalize_survey_data()` timezone handling for POSIXct columns.
* Fixed `%||%` operator import from rlang (was causing NAMESPACE conflict).
* Fixed `check_missing_by_variable()` vapply type mismatch.

# afcestDataCheck 0.2.0

## New features

* **GPS validation** (8 functions): boundary, centroid distance, accuracy,
  duplicates, lat/lon swap, null island, clustering, altitude.

* **Fabrication detection** (8 functions): Benford's Law (1st/2nd digit),
  digit preference, straightlining, ICC by enumerator, response entropy,
  duplicate response patterns, variance ratio.

* **Metadata checks** (6 functions): consent, form version, interview
  completion, survey tracking, ID format, respondent eligibility.

* **Logic consistency** (7 functions): household composition, income-
  expenditure ratio, age-DOB consistency, end-before-start, duration by
  household size, roster completeness, custom logic assertions.

* **Text quality** (5 functions): other specify, text length, text
  duplicates, name format, phone format.

* `run_all_checks()` orchestrator with YAML-driven configuration.

* `export_to_excel()` and `export_to_csv()` export functions.

# afcestDataCheck 0.1.0

## Initial release

* **Core infrastructure**: `new_check_result()`, `is_check_result()`,
  `bind_check_results()`, `read_config()`, `cfg_get()`.

* **Platform normalizer**: `normalize_survey_data()` with auto-detection
  for KoBoToolbox, SurveyCTO, ODK Central, Survey Solutions, and CSPro.
  `parse_gps_string()` for KoBo/ODK GPS string parsing.

* **Identification checks** (4 functions): duplicate IDs, fingerprint
  duplicates, missing IDs, ID in sampling frame.

* **Completeness checks** (4 functions): missing by variable, missing by
  enumerator, all-missing variables, skip pattern.

* **Outlier checks** (5 functions): IQR, z-score, MAD, hard range,
  negative values.

* **Enumerator performance checks** (4 functions): daily productivity,
  mean duration, DK rate, time gap.

* **Timing checks** (3 functions): survey duration, collection window,
  future dates.

* Example YAML config at `inst/example_config.yml`.
