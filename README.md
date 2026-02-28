# afcestDataCheck

Comprehensive high-frequency data quality checks for survey-based research in R. Built for agricultural, health, and socioeconomic surveys in development contexts.

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/afcest/survey-data-quality/actions)
[![Codecov](https://img.shields.io/badge/codecov-85%25-yellow)](https://codecov.io/gh/afcest/survey-data-quality)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-blue)](https://cran.r-project.org/)
<!-- badges: end -->

## Why afcestDataCheck?

Existing high-frequency check (HFC) tools are fragmented across platforms, limited in scope, and often locked into proprietary ecosystems. `afcestDataCheck` consolidates best practices from the World Bank, IPA, and UNHCR into a single R package with significantly broader coverage -- particularly in GPS validation, data fabrication detection, back-check integration, and multi-platform support. It provides 65+ check functions across 13 categories, supports 5 survey platforms (KoBoToolbox, SurveyCTO, ODK Central, Survey Solutions, CSPro), and is fully configurable via YAML.

## Installation

```r
# Install from GitHub
remotes::install_github("afcest/survey-data-quality")
```

## Quick Start

```r
library(afcestDataCheck)

# 1. Create sample data
survey <- data.frame(
  hh_id      = c("HH001", "HH002", "HH002", "HH003", "HH004"),
  enum_id    = c("E01", "E01", "E02", "E02", "E03"),
  duration   = c(45, 8, 30, 120, 250),
  gps_lat    = c(12.37, 12.38, 0, 12.36, 12.39),
  gps_lon    = c(-1.52, -1.51, 0, -1.50, -1.53),
  income     = c(50000, 30000, 45000, -500, 80000)
)

# 2. Run all checks from a YAML config
report <- run_all_checks(survey, config = "checks_config.yml")

# 3. Export results to Excel
export_to_excel(report$results, survey, id_col = "hh_id", path = "hfc_report.xlsx")
```

Or run individual checks directly:

```r
# Check for duplicate IDs
check_duplicate_ids(survey, id_col = "hh_id")

# Detect GPS null island
check_gps_null_island(survey, id_col = "hh_id", lat_col = "gps_lat", lon_col = "gps_lon")

# Benford's Law test for fabrication
check_benford_first_digit(survey, id_col = "hh_id", num_col = "income")
```

## Check Categories

The package provides **54 check functions** across **10 categories**:

### Identification (5 checks)
Duplicate IDs, fingerprint duplicates (same quasi-identifiers with different IDs), missing IDs, ID format validation against regex patterns, and ID-in-sampling-frame verification.

### Metadata (5 checks)
Consent validation, form version verification, interview completion status, survey tracking against per-stratum targets, and respondent eligibility (age/gender criteria).

### Completeness (5 checks)
Missing rate by variable (with configurable thresholds), missing rate by enumerator (z-score flagging), entirely missing variables, skip pattern consistency, and roster completeness (HH size vs. member count).

### Outliers (5 checks)
IQR method, robust z-score (median/MAD), modified z-score, hard range constraints, and negative value detection. All methods configurable per variable.

### Enumerator Performance (5 checks)
Daily productivity limits, mean duration deviation from team average, "don't know" response rates, time gaps between consecutive surveys, and straightlining detection (identical response patterns).

### Timing (5 checks)
Survey duration bounds, collection window enforcement, future date detection, end-before-start timestamp validation, and duration-by-household-size proportionality.

### GPS Validation (8 checks)
Boundary enforcement (bounding box or sf polygon), centroid distance (Haversine formula), accuracy threshold, coordinate duplicates, lat/lon swap detection, null island / default values, excessive clustering by enumerator, and altitude plausibility.

### Fabrication Detection (7 checks)
Benford's Law first-digit test, Benford's Law second-digit test, terminal digit preference (heaping), intraclass correlation coefficient (ICC) by enumerator, response entropy (Shannon), duplicate response patterns across surveys, and between/within variance ratio by enumerator.

### Logic Consistency (4 checks)
Household composition plausibility, income-expenditure ratio, age vs. date-of-birth consistency, and custom logic assertions via user-defined R expressions.

### Text Quality (5 checks)
"Other (specify)" write-in extraction for recoding, text response length validation, duplicated text detection (copy-paste), name format validation (test values, digits-only, single characters), and phone number format with country code prefix checking.

## Supported Platforms

`afcestDataCheck` auto-detects and normalizes data from five major survey platforms:

| Platform | Auto-detection | ID Column | Key Columns |
|---|---|---|---|
| **KoBoToolbox** | `_id`, `_submission_time` | `_id` | `_submitted_by`, GPS fields |
| **SurveyCTO** | `KEY`, `submissiondate` | `KEY` | `username`, `starttime` |
| **ODK Central** | `__id`, `__system/*` | `__id` | `submitterName`, dates |
| **Survey Solutions** | `interview__id` | `interview__id` | `responsible`, dates |
| **CSPro** | `guid`, `case_id` | `guid` | `operator`, `supervisor` |

```r
# Auto-detect platform and normalize column names
normalized <- normalize_survey_data(raw_data, platform = "auto")
```

## Configuration

Define your entire check suite in a single YAML file:

```yaml
project:
  name: "Household Survey 2025"
  survey_start: "2025-01-15"
  survey_end: "2025-06-30"

variables:
  id: "hh_id"
  enumerator: "enum_id"
  date: "submission_date"
  duration: "interview_duration"
  gps_latitude: "gps_lat"
  gps_longitude: "gps_lon"

checks:
  identification:
    duplicate_ids: true
    missing_ids: true
    id_format:
      enabled: true
      pattern: "^HH\\d{4}$"

  outliers:
    method: "iqr"
    multiplier: 1.5
    variables:
      - col: "income"
        min: 0
        max: 500000
      - col: "hh_size"
        min: 1
        max: 30

  fabrication:
    benford: true
    digit_preference: true
    icc:
      enabled: true
      max_icc: 0.15

  gps:
    accuracy:
      enabled: true
      max_accuracy: 50
    boundary:
      enabled: true
      bbox: { xmin: -5.5, xmax: 2.5, ymin: 4.5, ymax: 15.1 }
```

## Standardized Output

Every check function returns a `check_result` object with a consistent structure:

```r
list(
  check_name   = "A01_duplicate_id",
  check_category = "identification",
  n_flagged    = 2L,
  n_total      = 500L,
  flagged_ids  = c("HH002", "HH045"),
  flag_reason  = c("ID 'HH002' appears 3 times", "ID 'HH045' appears 2 times"),
  severity     = "error",
  summary_stat = list(n_unique = 498, n_duplicated_groups = 2)
)
```

This enables programmatic downstream processing, dashboard integration, and automated reporting.

## Export

```r
# Multi-sheet Excel workbook with conditional formatting
export_to_excel(report$results, data, id_col = "hh_id", path = "report.xlsx")

# CSV files (summary + flagged records)
export_to_csv(report$results, output_dir = "output/", prefix = "hfc_report")
```

## Contributing

Contributions are welcome. Please follow these guidelines:

1. Fork the repository and create a feature branch.
2. Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
3. Add tests for new check functions using `testthat`.
4. Ensure `devtools::check()` passes with 0 errors and 0 warnings.
5. Submit a pull request with a clear description of the change.

## License

MIT License. See [LICENSE](LICENSE) for details.

## About AfCEST

The **African Center for Studies and Training (AfCEST)** is a consulting firm registered in Burkina Faso, specializing in data systems, monitoring and evaluation, and applied research for international development projects across West and Central Africa. AfCEST builds open-source tools that strengthen data quality and evidence-based decision-making in development programs.
