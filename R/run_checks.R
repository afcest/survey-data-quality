# ============================================================================
# run_checks.R — Orchestrator: read config, run all enabled checks
# ============================================================================

#' Run all enabled checks from a configuration
#'
#' This is the main entry point for the package. It reads a config,
#' normalizes the data, and runs all enabled checks.
#'
#' @param data Data frame of survey submissions
#' @param config Either a path to a YAML config file or an adc_config object
#' @param sampling_frame Optional character vector of valid IDs
#' @param backcheck_data Optional data frame for back-check comparisons
#' @param verbose Logical. Show progress messages (default TRUE)
#' @param ... Reserved
#' @return A list of class "adc_report" containing all check results
#' @export
run_all_checks <- function(data, config, sampling_frame = NULL,
                           backcheck_data = NULL, verbose = TRUE, ...) {

  # -- Resolve config ----------------------------------------------------------
  if (is.character(config) && length(config) == 1L) {
    config <- read_config(config)
  }
  stopifnot(inherits(config, "adc_config"))

  # -- Variable mappings -------------------------------------------------------
  vars <- config$variables
  id_col        <- vars$id
  enum_col      <- vars$enumerator
  date_col      <- vars$date
  start_col     <- vars$start_time
  end_col       <- vars$end_time
  duration_col  <- vars$duration
  gps_var       <- vars$gps
  quasi_ids     <- vars$quasi_ids

  # Convenience: check whether a column exists in data

  has_col  <- function(col) !is.null(col) && col %in% names(data)
  has_cols <- function(cols) !is.null(cols) && all(cols %in% names(data))

  # -- Storage -----------------------------------------------------------------
  results <- list()
  errors  <- list()

  # Helper: safely run one check and accumulate result/error
  run_one <- function(name, expr, progress_id = NULL) {
    res <- tryCatch(
      expr,
      error = function(e) {
        errors[[name]] <<- conditionMessage(e)
        if (verbose) {
          cli::cli_alert_danger("Check {.field {name}} failed: {conditionMessage(e)}")
        }
        NULL
      }
    )
    if (!is.null(res) && is_check_result(res)) {
      results[[name]] <<- res
    }
    invisible(NULL)
  }

  # Helper: is a check enabled in config? Defaults to default_val.
  is_enabled <- function(..., default_val = TRUE) {
    val <- cfg_get(config, ..., default = default_val)
    # Handle both logical and list(enabled = TRUE/FALSE) patterns
    if (is.list(val)) {
      enabled <- val$enabled
      if (is.null(enabled)) return(default_val)
      return(isTRUE(enabled))
    }
    isTRUE(val)
  }

  # -- Count total checks to run (for progress bar) ---------------------------
  # We build a flat list of check descriptors, then iterate with a progress bar.

  check_registry <- list()
  add_check <- function(name, category, fn) {
    check_registry[[length(check_registry) + 1L]] <<- list(
      name = name, category = category, fn = fn
    )
  }

  # ==========================================================================
  # 1. IDENTIFICATION
  # ==========================================================================

  if (is_enabled("checks", "identification", "duplicate_ids") && has_col(id_col)) {
    add_check("A01_duplicate_id", "identification", function() {
      check_duplicate_ids(data, id_col)
    })
  }

  if (is_enabled("checks", "identification", "fingerprint_duplicates") &&
      has_col(id_col) && has_cols(quasi_ids)) {
    add_check("A02_fingerprint_duplicate", "identification", function() {
      check_duplicate_fingerprint(data, id_col, quasi_ids)
    })
  }

  if (is_enabled("checks", "identification", "missing_ids") && has_col(id_col)) {
    add_check("A03_missing_id", "identification", function() {
      check_missing_ids(data, id_col)
    })
  }

  if (is_enabled("checks", "identification", "id_in_sample", default_val = FALSE) &&
      has_col(id_col) && !is.null(sampling_frame)) {
    add_check("A05_id_not_in_sample", "identification", function() {
      check_id_in_sample(data, id_col, sampling_frame)
    })
  }

  if (is_enabled("checks", "identification", "id_format", default_val = FALSE) &&
      has_col(id_col)) {
    id_pattern <- cfg_get(config, "checks", "identification", "id_format",
                          "pattern", default = NULL)
    if (!is.null(id_pattern)) {
      add_check("A04_id_format", "identification", function() {
        check_id_format(data, id_col, id_pattern)
      })
    }
  }

  # ==========================================================================
  # 2. METADATA
  # ==========================================================================

  # Consent
  consent_col <- cfg_get(config, "variables", "consent", default = NULL)
  if (is_enabled("checks", "metadata", "consent", default_val = FALSE) &&
      has_col(id_col) && has_col(consent_col)) {
    consent_value <- cfg_get(config, "checks", "metadata", "consent",
                             "consent_value", default = 1)
    add_check("A06_consent", "metadata", function() {
      check_consent(data, id_col, consent_col, consent_value = consent_value)
    })
  }

  # Form version
  version_col <- cfg_get(config, "variables", "form_version", default = NULL)
  if (is_enabled("checks", "metadata", "form_version", default_val = FALSE) &&
      has_col(id_col) && has_col(version_col)) {
    expected_version <- cfg_get(config, "checks", "metadata", "form_version",
                                "expected_version", default = NULL)
    if (!is.null(expected_version)) {
      add_check("A07_form_version", "metadata", function() {
        check_form_version(data, id_col, version_col,
                           expected_version = expected_version)
      })
    }
  }

  # Interview completed
  status_col <- cfg_get(config, "variables", "interview_status", default = NULL)
  if (is_enabled("checks", "metadata", "interview_completed", default_val = FALSE) &&
      has_col(id_col) && has_col(status_col)) {
    complete_val <- cfg_get(config, "checks", "metadata", "interview_completed",
                            "complete_value", default = "complete")
    add_check("A09_interview_completed", "metadata", function() {
      check_interview_completed(data, id_col, status_col,
                                complete_value = complete_val)
    })
  }

  # Survey tracking
  strata_col <- cfg_get(config, "variables", "strata", default = NULL)
  if (is_enabled("checks", "metadata", "survey_tracking", default_val = FALSE) &&
      has_col(id_col) && has_col(strata_col)) {
    targets <- cfg_get(config, "checks", "metadata", "survey_tracking",
                       "target_per_stratum", default = NULL)
    if (!is.null(targets)) {
      add_check("A10_survey_tracking", "metadata", function() {
        check_survey_tracking(data, id_col, strata_col,
                              target_per_stratum = targets)
      })
    }
  }

  # Respondent eligibility
  if (is_enabled("checks", "metadata", "respondent_eligibility", default_val = FALSE) &&
      has_col(id_col)) {
    elig_cfg <- cfg_get(config, "checks", "metadata", "respondent_eligibility",
                        default = list())
    age_col_elig    <- cfg_get(config, "variables", "respondent_age", default = NULL)
    gender_col_elig <- cfg_get(config, "variables", "respondent_gender", default = NULL)
    if (has_col(age_col_elig) || has_col(gender_col_elig)) {
      add_check("A12_respondent_eligibility", "metadata", function() {
        check_respondent_eligibility(
          data, id_col,
          age_col        = if (has_col(age_col_elig)) age_col_elig else NULL,
          min_age        = cfg_get(config, "checks", "metadata",
                                   "respondent_eligibility", "min_age", default = 18),
          gender_col     = if (has_col(gender_col_elig)) gender_col_elig else NULL,
          eligible_gender = elig_cfg$eligible_gender
        )
      })
    }
  }

  # ==========================================================================
  # 3. COMPLETENESS
  # ==========================================================================

  # Missing by variable
  if (is_enabled("checks", "completeness", "missing_by_variable") && has_col(id_col)) {
    miss_cfg <- cfg_get(config, "checks", "completeness", "missing_by_variable",
                        default = list())
    miss_thresh <- if (is.list(miss_cfg)) {
      miss_cfg$threshold %||% 0.05
    } else 0.05
    exclude_cols <- if (is.list(miss_cfg)) miss_cfg$exclude_cols else NULL
    add_check("C01_missing_by_variable", "completeness", function() {
      check_missing_by_variable(data, id_col,
                                threshold = miss_thresh,
                                exclude_cols = exclude_cols)
    })
  }

  # Missing by enumerator
  if (is_enabled("checks", "completeness", "missing_by_enumerator") &&
      has_col(id_col) && has_col(enum_col)) {
    miss_enum_cfg <- cfg_get(config, "checks", "completeness",
                             "missing_by_enumerator", default = list())
    miss_enum_sd <- if (is.list(miss_enum_cfg)) {
      miss_enum_cfg$threshold_sd %||% 2
    } else 2
    add_check("C02_missing_by_enumerator", "completeness", function() {
      check_missing_by_enumerator(data, id_col, enum_col,
                                  threshold_sd = miss_enum_sd)
    })
  }

  # All missing variables
  if (is_enabled("checks", "completeness", "all_missing_variables")) {
    add_check("C04_all_missing_variable", "completeness", function() {
      check_all_missing_variables(data)
    })
  }

  # Skip patterns
  skip_patterns <- cfg_get(config, "skip_patterns", default = NULL)
  if (!is.null(skip_patterns) && has_col(id_col)) {
    for (sp_i in seq_along(skip_patterns)) {
      sp <- skip_patterns[[sp_i]]
      if (has_col(sp$parent_col) && has_col(sp$child_col)) {
        # Capture sp locally via a factory
        local({
          sp_local <- sp
          idx <- sp_i
          add_check(paste0("C03_skip_pattern_", idx), "completeness", function() {
            check_skip_pattern(
              data, id_col,
              parent_col      = sp_local$parent_col,
              child_col       = sp_local$child_col,
              parent_value    = sp_local$parent_value,
              expect_child_na = sp_local$expect_child_na %||% TRUE
            )
          })
        })
      }
    }
  }

  # Roster completeness
  hh_size_col     <- cfg_get(config, "variables", "hh_size", default = NULL)
  member_count_col <- cfg_get(config, "variables", "roster_member_count", default = NULL)
  if (is_enabled("checks", "completeness", "roster_completeness", default_val = FALSE) &&
      has_col(id_col) && has_col(hh_size_col) && has_col(member_count_col)) {
    add_check("C07_roster_completeness", "completeness", function() {
      check_roster_completeness(data, id_col, hh_size_col, member_count_col)
    })
  }

  # ==========================================================================
  # 4. OUTLIERS (per-variable from config$checks$outliers$variables)
  # ==========================================================================

  outlier_method <- cfg_get(config, "checks", "outliers", "method", default = "iqr")
  outlier_multiplier <- cfg_get(config, "checks", "outliers", "multiplier", default = 1.5)
  outlier_threshold  <- cfg_get(config, "checks", "outliers", "threshold", default = 3)
  outlier_vars <- cfg_get(config, "checks", "outliers", "variables", default = list())

  for (ov_i in seq_along(outlier_vars)) {
    local({
      ov <- outlier_vars[[ov_i]]
      ov_col <- ov$col
      if (!is.null(ov_col) && has_col(ov_col)) {
        # Hard range check if min/max specified
        if (!is.null(ov$min) || !is.null(ov$max)) {
          add_check(paste0("D04_hard_range_", ov_col), "outliers", function() {
            check_hard_range(data, id_col, ov_col,
                             min_val = ov$min %||% -Inf,
                             max_val = ov$max %||% Inf)
          })
        }

        # Statistical outlier check
        if (outlier_method == "iqr") {
          add_check(paste0("D01_outlier_iqr_", ov_col), "outliers", function() {
            check_outliers_iqr(data, id_col, ov_col,
                               multiplier = outlier_multiplier)
          })
        } else if (outlier_method == "zscore") {
          add_check(paste0("D02_outlier_zscore_", ov_col), "outliers", function() {
            check_outliers_zscore(data, id_col, ov_col,
                                 threshold = outlier_threshold)
          })
        } else if (outlier_method == "mad") {
          add_check(paste0("D03_outlier_mad_", ov_col), "outliers", function() {
            check_outliers_mad(data, id_col, ov_col,
                               threshold = outlier_threshold)
          })
        }
      }
    })
  }

  # ==========================================================================
  # 5. ENUMERATOR
  # ==========================================================================

  # Productivity
  if (is_enabled("checks", "enumerator", "productivity", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col) && has_col(date_col)) {
    max_daily <- cfg_get(config, "checks", "enumerator", "productivity",
                         "max_daily", default = 10)
    add_check("B01_enumerator_productivity", "enumerator", function() {
      check_enumerator_productivity(data, id_col, enum_col, date_col,
                                    max_daily = max_daily)
    })
  }

  # Duration
  if (is_enabled("checks", "enumerator", "duration", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col) && has_col(duration_col)) {
    enum_dur_sd <- cfg_get(config, "checks", "enumerator", "duration",
                           "threshold_sd", default = 2)
    add_check("B02_enumerator_duration", "enumerator", function() {
      check_enumerator_duration(data, id_col, enum_col, duration_col,
                                threshold_sd = enum_dur_sd)
    })
  }

  # DK rate
  if (is_enabled("checks", "enumerator", "dk_rate", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col)) {
    dk_vals <- cfg_get(config, "checks", "enumerator", "dk_rate",
                       "dk_values", default = "dk")
    dk_sd   <- cfg_get(config, "checks", "enumerator", "dk_rate",
                       "threshold_sd", default = 2)
    add_check("B03_dk_rate", "enumerator", function() {
      check_dk_rate(data, id_col, enum_col,
                    dk_value = dk_vals, threshold_sd = dk_sd)
    })
  }

  # Time gap
  if (is_enabled("checks", "enumerator", "time_gap", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col) && has_col(start_col)) {
    min_gap <- cfg_get(config, "checks", "enumerator", "time_gap",
                       "min_gap_minutes", default = 5)
    add_check("B08_enumerator_time_gap", "enumerator", function() {
      check_enumerator_time_gap(data, id_col, enum_col, start_col,
                                min_gap_minutes = min_gap)
    })
  }

  # Straightlining
  straightline_cols <- cfg_get(config, "checks", "enumerator", "straightlining",
                               "check_cols", default = NULL)
  if (is_enabled("checks", "enumerator", "straightlining", default_val = FALSE) &&
      has_col(id_col) && !is.null(straightline_cols) && has_cols(straightline_cols)) {
    max_identical <- cfg_get(config, "checks", "enumerator", "straightlining",
                             "max_identical_rate", default = 0.8)
    add_check("B09_straightlining", "enumerator", function() {
      check_straightlining(data, id_col, straightline_cols,
                           max_identical_rate = max_identical)
    })
  }

  # ==========================================================================
  # 6. TIMING
  # ==========================================================================

  # Survey duration
  if (is_enabled("checks", "timing", "duration") &&
      has_col(id_col) && has_col(duration_col)) {
    dur_cfg <- cfg_get(config, "checks", "timing", "duration", default = list())
    min_min <- if (is.list(dur_cfg)) dur_cfg$min_minutes %||% 15 else 15
    max_min <- if (is.list(dur_cfg)) dur_cfg$max_minutes %||% 120 else 120
    add_check("F01_survey_duration", "timing", function() {
      check_survey_duration(data, id_col, duration_col,
                            min_duration = min_min, max_duration = max_min)
    })
  }

  # Collection window
  if (is_enabled("checks", "timing", "collection_window") &&
      has_col(id_col) && has_col(date_col)) {
    survey_start <- cfg_get(config, "project", "survey_start", default = NULL)
    survey_end   <- cfg_get(config, "project", "survey_end", default = NULL)
    if (!is.null(survey_start) && !is.null(survey_end)) {
      add_check("F02_collection_window", "timing", function() {
        check_collection_window(data, id_col, date_col,
                                start_date = survey_start, end_date = survey_end)
      })
    }
  }

  # Future dates
  if (is_enabled("checks", "timing", "future_dates") &&
      has_col(id_col) && has_col(date_col)) {
    add_check("F03_future_date", "timing", function() {
      check_future_dates(data, id_col, date_col)
    })
  }

  # End before start
  if (is_enabled("checks", "timing", "end_before_start", default_val = TRUE) &&
      has_col(id_col) && has_col(start_col) && has_col(end_col)) {
    add_check("F04_end_before_start", "timing", function() {
      check_survey_end_before_start(data, id_col, start_col, end_col)
    })
  }

  # Duration by HH size
  if (is_enabled("checks", "timing", "duration_by_hh_size", default_val = FALSE) &&
      has_col(id_col) && has_col(duration_col) && has_col(hh_size_col)) {
    min_per_member <- cfg_get(config, "checks", "timing", "duration_by_hh_size",
                              "min_minutes_per_member", default = 3)
    add_check("F06_duration_by_hh_size", "timing", function() {
      check_duration_by_hh_size(data, id_col, duration_col, hh_size_col,
                                min_minutes_per_member = min_per_member)
    })
  }

  # ==========================================================================
  # 7. GPS
  # ==========================================================================

  lat_col <- cfg_get(config, "variables", "gps_latitude", default = NULL)
  lon_col <- cfg_get(config, "variables", "gps_longitude", default = NULL)
  acc_col <- cfg_get(config, "variables", "gps_accuracy", default = NULL)
  alt_col <- cfg_get(config, "variables", "gps_altitude", default = NULL)
  cluster_col <- cfg_get(config, "variables", "cluster", default = NULL)

  # GPS accuracy
  if (is_enabled("checks", "gps", "accuracy", default_val = FALSE) &&
      has_col(id_col) && has_col(acc_col)) {
    max_acc <- cfg_get(config, "checks", "gps", "accuracy",
                       "max_accuracy", default = 50)
    add_check("E03_gps_accuracy", "gps", function() {
      check_gps_accuracy(data, id_col, acc_col, max_accuracy = max_acc)
    })
  }

  # GPS duplicates
  if (is_enabled("checks", "gps", "duplicates", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col)) {
    min_dist <- cfg_get(config, "checks", "gps", "duplicates",
                        "min_distance", default = 0.0001)
    add_check("E04_gps_duplicates", "gps", function() {
      check_gps_duplicates(data, id_col, lat_col, lon_col,
                           min_distance = min_dist)
    })
  }

  # Null island
  if (is_enabled("checks", "gps", "null_island", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col)) {
    add_check("E07_gps_null_island", "gps", function() {
      check_gps_null_island(data, id_col, lat_col, lon_col)
    })
  }

  # Boundary
  if (is_enabled("checks", "gps", "boundary", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col)) {
    bbox <- cfg_get(config, "checks", "gps", "boundary", "bbox", default = NULL)
    add_check("E01_gps_boundary", "gps", function() {
      check_gps_boundary(data, id_col, lat_col, lon_col,
                         boundary_bbox = bbox)
    })
  }

  # Centroid distance
  if (is_enabled("checks", "gps", "centroid_distance", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col) &&
      has_col(cluster_col)) {
    max_dist <- cfg_get(config, "checks", "gps", "centroid_distance",
                        "max_distance_km", default = 10)
    add_check("E02_gps_centroid_distance", "gps", function() {
      check_gps_centroid_distance(data, id_col, lat_col, lon_col,
                                  cluster_col, max_distance_km = max_dist)
    })
  }

  # GPS swap
  if (is_enabled("checks", "gps", "swap", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col)) {
    expected_bbox <- cfg_get(config, "checks", "gps", "swap",
                             "expected_bbox", default = NULL)
    add_check("E05_gps_swap", "gps", function() {
      check_gps_swap(data, id_col, lat_col, lon_col,
                     expected_bbox = expected_bbox)
    })
  }

  # GPS clustering
  if (is_enabled("checks", "gps", "clustering", default_val = FALSE) &&
      has_col(id_col) && has_col(lat_col) && has_col(lon_col)) {
    max_same <- cfg_get(config, "checks", "gps", "clustering",
                        "max_same_point", default = 3)
    add_check("E08_gps_clustering", "gps", function() {
      check_gps_clustering(data, id_col, lat_col, lon_col,
                           enum_col = if (has_col(enum_col)) enum_col else NULL,
                           max_same_point = max_same)
    })
  }

  # GPS altitude
  if (is_enabled("checks", "gps", "altitude", default_val = FALSE) &&
      has_col(id_col) && has_col(alt_col)) {
    min_alt <- cfg_get(config, "checks", "gps", "altitude",
                       "min_alt", default = -500)
    max_alt <- cfg_get(config, "checks", "gps", "altitude",
                       "max_alt", default = 6000)
    add_check("E10_gps_altitude", "gps", function() {
      check_gps_altitude(data, id_col, alt_col,
                         min_alt = min_alt, max_alt = max_alt)
    })
  }

  # ==========================================================================
  # 8. FABRICATION
  # ==========================================================================

  fab_num_cols <- cfg_get(config, "checks", "fabrication", "num_cols", default = NULL)
  # Fall back to outlier variable columns if fabrication columns not specified
  if (is.null(fab_num_cols) && length(outlier_vars) > 0) {
    fab_num_cols <- vapply(outlier_vars, function(ov) ov$col, character(1))
    fab_num_cols <- fab_num_cols[fab_num_cols %in% names(data)]
  }

  # Benford first digit
  if (is_enabled("checks", "fabrication", "benford", default_val = FALSE) &&
      has_col(id_col) && !is.null(fab_num_cols)) {
    for (fc_i in seq_along(fab_num_cols)) {
      local({
        fc <- fab_num_cols[fc_i]
        if (has_col(fc)) {
          add_check(paste0("M01_benford_", fc), "fabrication", function() {
            check_benford_first_digit(data, id_col, fc)
          })
        }
      })
    }
  }

  # Digit preference
  if (is_enabled("checks", "fabrication", "digit_preference", default_val = FALSE) &&
      has_col(id_col) && !is.null(fab_num_cols)) {
    for (fc_i in seq_along(fab_num_cols)) {
      local({
        fc <- fab_num_cols[fc_i]
        if (has_col(fc)) {
          add_check(paste0("M03_digit_preference_", fc), "fabrication", function() {
            check_digit_preference(data, id_col, fc)
          })
        }
      })
    }
  }

  # ICC by enumerator
  if (is_enabled("checks", "fabrication", "icc", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col) && !is.null(fab_num_cols)) {
    valid_fab_cols <- fab_num_cols[fab_num_cols %in% names(data)]
    if (length(valid_fab_cols) > 0) {
      max_icc <- cfg_get(config, "checks", "fabrication", "icc",
                         "max_icc", default = 0.15)
      add_check("M04_icc_enumerator", "fabrication", function() {
        check_icc_enumerator(data, id_col, enum_col, valid_fab_cols,
                             max_icc = max_icc)
      })
    }
  }

  # Variance ratio
  if (is_enabled("checks", "fabrication", "variance_ratio", default_val = FALSE) &&
      has_col(id_col) && has_col(enum_col) && !is.null(fab_num_cols)) {
    valid_fab_cols <- fab_num_cols[fab_num_cols %in% names(data)]
    if (length(valid_fab_cols) > 0) {
      max_ratio <- cfg_get(config, "checks", "fabrication", "variance_ratio",
                           "max_ratio", default = 2)
      add_check("M08_variance_ratio", "fabrication", function() {
        check_variance_ratio(data, id_col, enum_col, valid_fab_cols,
                             max_ratio = max_ratio)
      })
    }
  }

  # Duplicate response patterns
  dup_check_cols <- cfg_get(config, "checks", "fabrication", "duplicate_patterns",
                            "check_cols", default = NULL)
  if (is_enabled("checks", "fabrication", "duplicate_patterns", default_val = FALSE) &&
      has_col(id_col) && !is.null(dup_check_cols) && has_cols(dup_check_cols)) {
    sim_thresh <- cfg_get(config, "checks", "fabrication", "duplicate_patterns",
                          "similarity_threshold", default = 0.95)
    add_check("M07_duplicate_response_patterns", "fabrication", function() {
      check_duplicate_response_patterns(data, id_col, dup_check_cols,
                                        similarity_threshold = sim_thresh)
    })
  }

  # Response entropy
  entropy_cols <- cfg_get(config, "checks", "fabrication", "response_entropy",
                          "check_cols", default = NULL)
  if (is_enabled("checks", "fabrication", "response_entropy", default_val = FALSE) &&
      has_col(id_col) && !is.null(entropy_cols) && has_cols(entropy_cols)) {
    min_ent <- cfg_get(config, "checks", "fabrication", "response_entropy",
                       "min_entropy_ratio", default = 0.3)
    add_check("M06_response_entropy", "fabrication", function() {
      check_response_entropy(data, id_col, entropy_cols,
                             min_entropy_ratio = min_ent)
    })
  }

  # ==========================================================================
  # 9. LOGIC
  # ==========================================================================

  # HH composition
  if (is_enabled("checks", "logic", "hh_composition", default_val = FALSE) &&
      has_col(id_col) && has_col(hh_size_col)) {
    roster_col_logic <- cfg_get(config, "variables", "roster_count", default = NULL)
    add_check("G01_hh_composition", "logic", function() {
      check_hh_composition(
        data, id_col, hh_size_col,
        roster_count_col = if (has_col(roster_col_logic)) roster_col_logic else NULL
      )
    })
  }

  # Income-expenditure
  income_col <- cfg_get(config, "variables", "income", default = NULL)
  expend_col <- cfg_get(config, "variables", "expenditure", default = NULL)
  if (is_enabled("checks", "logic", "income_expenditure", default_val = FALSE) &&
      has_col(id_col) && has_col(income_col) && has_col(expend_col)) {
    ie_ratio <- cfg_get(config, "checks", "logic", "income_expenditure",
                        "max_ratio", default = 5)
    add_check("G04_income_expenditure", "logic", function() {
      check_income_expenditure(data, id_col, income_col, expend_col,
                               max_ratio = ie_ratio)
    })
  }

  # Age-date consistency
  age_col_logic <- cfg_get(config, "variables", "age", default = NULL)
  dob_col       <- cfg_get(config, "variables", "dob", default = NULL)
  if (is_enabled("checks", "logic", "age_date_consistency", default_val = FALSE) &&
      has_col(id_col) && has_col(age_col_logic) && has_col(dob_col)) {
    tol_years <- cfg_get(config, "checks", "logic", "age_date_consistency",
                         "tolerance_years", default = 1)
    add_check("F09_age_date_consistency", "logic", function() {
      check_age_date_consistency(data, id_col, age_col_logic, dob_col,
                                 tolerance_years = tol_years)
    })
  }

  # Custom logic assertions
  custom_logic <- cfg_get(config, "checks", "logic", "custom", default = NULL)
  if (!is.null(custom_logic) && has_col(id_col)) {
    for (cl_i in seq_along(custom_logic)) {
      local({
        cl <- custom_logic[[cl_i]]
        cl_idx <- cl_i
        if (!is.null(cl$condition)) {
          add_check(paste0("G12_custom_logic_", cl_idx), "logic", function() {
            check_custom_logic(data, id_col,
                               condition_expr = cl$condition,
                               description = cl$description %||% "Custom logic check")
          })
        }
      })
    }
  }

  # ==========================================================================
  # EXECUTE ALL REGISTERED CHECKS
  # ==========================================================================

  n_checks <- length(check_registry)

  if (verbose) {
    project_name <- cfg_get(config, "project", "name", default = "Survey")
    cli::cli_h1("afcestDataCheck: {project_name}")
    cli::cli_alert_info("{nrow(data)} observations, {n_checks} checks to run")
  }

  if (n_checks > 0L && verbose) {
    cli::cli_progress_bar(
      "Running checks",
      total = n_checks,
      format = "{cli::pb_spin} Running check {cli::pb_current}/{cli::pb_total} [{cli::pb_name}] {cli::pb_bar} {cli::pb_percent}"
    )
  }

  for (i in seq_along(check_registry)) {
    entry <- check_registry[[i]]
    if (verbose) {
      cli::cli_progress_update(.envir = parent.frame())
    }
    run_one(entry$name, entry$fn())
  }

  if (n_checks > 0L && verbose) {
    cli::cli_progress_done()
  }

  # -- Build report ------------------------------------------------------------
  result_list <- results
  n_checks_run    <- length(result_list)
  n_checks_failed <- sum(vapply(result_list, function(r) r$n_flagged > 0L,
                                logical(1)))

  summary_tbl <- if (n_checks_run > 0L) {
    bind_check_results(result_list)
  } else {
    bind_check_results()
  }

  report <- structure(
    list(
      results        = result_list,
      summary        = summary_tbl,
      config         = config,
      n_checks_run   = as.integer(n_checks_run),
      n_checks_failed = as.integer(n_checks_failed),
      errors         = errors,
      timestamp      = Sys.time()
    ),
    class = "adc_report"
  )

  if (verbose) {
    cli::cli_alert_success(
      "Done: {n_checks_run} checks run, {n_checks_failed} with flags, {length(errors)} errors"
    )
  }

  report
}


# ============================================================================
# print.adc_report
# ============================================================================

#' Print an adc_report summary
#'
#' @param x An adc_report object
#' @param ... Ignored
#' @export
print.adc_report <- function(x, ...) {
  project_name <- cfg_get(x$config, "project", "name", default = "Survey")

  cli::cli_h1("afcestDataCheck Report: {project_name}")
  cli::cli_text("Timestamp: {format(x$timestamp, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_text("Checks run: {x$n_checks_run}")
  cli::cli_text("Checks with flags: {x$n_checks_failed}")
  cli::cli_text("Check errors: {length(x$errors)}")


  if (length(x$errors) > 0L) {
    cli::cli_h2("Errors")
    for (nm in names(x$errors)) {
      cli::cli_alert_danger("{nm}: {x$errors[[nm]]}")
    }
  }

  # Show flagged checks sorted by severity
  if (nrow(x$summary) > 0L) {
    flagged <- x$summary[x$summary$n_flagged > 0L, , drop = FALSE]
    if (nrow(flagged) > 0L) {
      # Sort: errors first, then warnings, then info
      severity_order <- c(error = 1L, warning = 2L, info = 3L)
      flagged$sev_order <- severity_order[flagged$severity]
      flagged <- flagged[order(flagged$sev_order, -flagged$n_flagged), ]
      flagged$sev_order <- NULL

      cli::cli_h2("Flagged Checks ({nrow(flagged)})")

      for (i in seq_len(nrow(flagged))) {
        row <- flagged[i, ]
        icon <- switch(row$severity, error = "x", warning = "!", info = "i")
        cli::cli_text(
          "[{icon}] {row$check_name} ({row$check_category}): ",
          "{row$n_flagged}/{row$n_total} flagged ({row$pct_flagged}%) ",
          "[{row$severity}]"
        )
      }
    } else {
      cli::cli_alert_success("No flags raised. Data looks clean!")
    }
  }

  invisible(x)
}


# ============================================================================
# summary.adc_report
# ============================================================================

#' Summarize an adc_report
#'
#' Returns the summary tibble produced by [bind_check_results()].
#'
#' @param object An adc_report object
#' @param ... Ignored
#' @return A tibble summarizing all checks
#' @export
summary.adc_report <- function(object, ...) {
  object$summary
}
