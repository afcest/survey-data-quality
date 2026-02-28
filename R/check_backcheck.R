#' Check back-check coverage
#'
#' J.01: Verify that a sufficient percentage of original surveys have been
#' back-checked. If strata are provided, coverage is computed per stratum.
#'
#' @param data Data frame of original survey submissions
#' @param id_col Character. Name of the primary key column in original data
#' @param backcheck_data Data frame of back-check submissions
#' @param bc_id_col Character. Name of the ID column in backcheck_data that
#'   matches records in data
#' @param strata_col Character or NULL. Column in data for stratified coverage
#'   (e.g., region, district). Default NULL computes overall coverage only.
#' @param min_coverage Numeric. Minimum required coverage rate (default 0.10)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_backcheck_coverage <- function(data, id_col, backcheck_data, bc_id_col,
                                     strata_col = NULL, min_coverage = 0.10,
                                     ...) {
  assert_columns(data, id_col, context = "check_backcheck_coverage")
  assert_columns(backcheck_data, bc_id_col, context = "check_backcheck_coverage")
  if (!is.null(strata_col)) {
    assert_columns(data, strata_col, context = "check_backcheck_coverage")
  }

  orig_ids <- as_id(data[[id_col]])
  bc_ids   <- as_id(backcheck_data[[bc_id_col]])

  if (is.null(strata_col)) {
    # Overall coverage
    n_original    <- length(unique(orig_ids[!is.na(orig_ids)]))
    n_backchecked <- length(unique(bc_ids[bc_ids %in% orig_ids & !is.na(bc_ids)]))
    coverage_pct  <- if (n_original > 0) n_backchecked / n_original else 0

    coverage_stats <- dplyr::tibble(
      stratum       = "overall",
      n_original    = as.integer(n_original),
      n_backchecked = as.integer(n_backchecked),
      coverage_pct  = round(coverage_pct, 4)
    )

    below <- coverage_stats |> dplyr::filter(coverage_pct < min_coverage)
    flagged_ids <- below$stratum
    flag_reason <- if (nrow(below) > 0) {
      paste0("Coverage ", round(below$coverage_pct * 100, 1),
             "% is below minimum ", min_coverage * 100, "%")
    } else {
      character()
    }
  } else {
    # Per-stratum coverage
    strata_map <- dplyr::tibble(
      id      = orig_ids,
      stratum = as_id(data[[strata_col]])
    ) |> dplyr::filter(!is.na(id), !is.na(stratum))

    bc_matched <- dplyr::tibble(id = bc_ids) |>
      dplyr::filter(id %in% strata_map$id) |>
      dplyr::left_join(strata_map, by = "id")

    n_by_stratum <- strata_map |>
      dplyr::group_by(stratum) |>
      dplyr::summarise(n_original = dplyr::n_distinct(id), .groups = "drop")

    bc_by_stratum <- bc_matched |>
      dplyr::group_by(stratum) |>
      dplyr::summarise(n_backchecked = dplyr::n_distinct(id), .groups = "drop")

    coverage_stats <- n_by_stratum |>
      dplyr::left_join(bc_by_stratum, by = "stratum") |>
      dplyr::mutate(
        n_backchecked = ifelse(is.na(n_backchecked), 0L, n_backchecked),
        coverage_pct  = round(
          ifelse(n_original > 0, n_backchecked / n_original, 0), 4
        )
      )

    below <- coverage_stats |> dplyr::filter(coverage_pct < min_coverage)
    flagged_ids <- as.character(below$stratum)
    flag_reason <- if (nrow(below) > 0) {
      paste0("Stratum '", below$stratum, "' coverage ",
             round(below$coverage_pct * 100, 1),
             "% is below minimum ", min_coverage * 100, "%")
    } else {
      character()
    }
  }

  new_check_result(
    check_name     = "J01_backcheck_coverage",
    check_category = "backcheck",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(nrow(coverage_stats)),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      coverage_stats = coverage_stats
    )
  )
}

#' Check Type 1 variable matches in back-checks
#'
#' J.02: Type 1 variables (e.g., gender, location) should NEVER change between
#' the original survey and the back-check. Any mismatch signals a serious data
#' quality issue.
#'
#' @param data Data frame of original survey submissions
#' @param id_col Character. Primary key column in original data
#' @param backcheck_data Data frame of back-check submissions
#' @param bc_id_col Character. ID column in backcheck_data matching data
#' @param t1_cols Character vector. Type 1 variable names (must exist in both
#'   data and backcheck_data)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_backcheck_t1_match <- function(data, id_col, backcheck_data, bc_id_col,
                                     t1_cols, ...) {
  assert_columns(data, c(id_col, t1_cols),
                 context = "check_backcheck_t1_match")
  assert_columns(backcheck_data, c(bc_id_col, t1_cols),
                 context = "check_backcheck_t1_match")

  orig_ids <- as_id(data[[id_col]])
  bc_ids   <- as_id(backcheck_data[[bc_id_col]])

  # Find matched pairs
 matched_ids <- intersect(
    orig_ids[!is.na(orig_ids)],
    bc_ids[!is.na(bc_ids)]
  )

  if (length(matched_ids) == 0L) {
    return(new_check_result(
      check_name     = "J02_backcheck_t1_match",
      check_category = "backcheck",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "error",
      summary_stat   = list(
        match_rate_by_variable = dplyr::tibble(
          variable = character(), n_compared = integer(),
          n_matched = integer(), match_rate = numeric()
        ),
        overall_match_rate = NA_real_
      )
    ))
  }

  # Align data by matched IDs
  orig_matched <- data[match(matched_ids, orig_ids), , drop = FALSE]
  bc_matched   <- backcheck_data[match(matched_ids, bc_ids), , drop = FALSE]

  # Compare each T1 variable
  per_var <- vapply(t1_cols, function(col) {
    orig_vals <- as.character(orig_matched[[col]])
    bc_vals   <- as.character(bc_matched[[col]])
    sum(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals)),
        na.rm = FALSE)
  }, integer(1))

  match_rate_by_variable <- dplyr::tibble(
    variable   = t1_cols,
    n_compared = as.integer(length(matched_ids)),
    n_matched  = as.integer(per_var),
    match_rate = round(n_matched / n_compared, 4)
  )

  overall_match_rate <- round(
    sum(match_rate_by_variable$n_matched) /
      sum(match_rate_by_variable$n_compared), 4
  )

  # Identify IDs with any T1 mismatch
  mismatch_matrix <- vapply(t1_cols, function(col) {
    orig_vals <- as.character(orig_matched[[col]])
    bc_vals   <- as.character(bc_matched[[col]])
    !(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals)))
  }, logical(length(matched_ids)))

  if (is.null(dim(mismatch_matrix))) {
    # Single T1 column: mismatch_matrix is a vector
    any_mismatch <- mismatch_matrix
    mismatch_cols_per_id <- ifelse(mismatch_matrix, t1_cols, NA_character_)
  } else {
    any_mismatch <- rowSums(mismatch_matrix) > 0
    mismatch_cols_per_id <- apply(mismatch_matrix, 1, function(row) {
      paste(t1_cols[row], collapse = ", ")
    })
  }

  flagged_ids <- matched_ids[any_mismatch]
  flag_reason <- if (length(flagged_ids) > 0) {
    paste0("Mismatch on: ", mismatch_cols_per_id[any_mismatch])
  } else {
    character()
  }

  new_check_result(
    check_name     = "J02_backcheck_t1_match",
    check_category = "backcheck",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(length(matched_ids)),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      match_rate_by_variable = match_rate_by_variable,
      overall_match_rate     = overall_match_rate
    )
  )
}

#' Check Type 2 variable matches in back-checks
#'
#' J.03: Type 2 variables can legitimately change but are used to assess
#' enumerator quality. High error rates indicate potential enumerator problems.
#'
#' @param data Data frame of original survey submissions
#' @param id_col Character. Primary key column in original data
#' @param backcheck_data Data frame of back-check submissions
#' @param bc_id_col Character. ID column in backcheck_data matching data
#' @param t2_cols Character vector. Type 2 variable names (must exist in both
#'   data and backcheck_data)
#' @param max_error_rate Numeric. Maximum acceptable per-enumerator error rate
#'   (default 0.10)
#' @param enum_col Character or NULL. Enumerator column in original data. If
#'   NULL, per-enumerator analysis is skipped.
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_backcheck_t2_match <- function(data, id_col, backcheck_data, bc_id_col,
                                     t2_cols, max_error_rate = 0.10,
                                     enum_col = NULL, ...) {
  required_cols <- c(id_col, t2_cols)
  if (!is.null(enum_col)) required_cols <- c(required_cols, enum_col)
  assert_columns(data, required_cols, context = "check_backcheck_t2_match")
  assert_columns(backcheck_data, c(bc_id_col, t2_cols),
                 context = "check_backcheck_t2_match")

  orig_ids <- as_id(data[[id_col]])
  bc_ids   <- as_id(backcheck_data[[bc_id_col]])

  matched_ids <- intersect(
    orig_ids[!is.na(orig_ids)],
    bc_ids[!is.na(bc_ids)]
  )

  if (length(matched_ids) == 0L) {
    return(new_check_result(
      check_name     = "J03_backcheck_t2_match",
      check_category = "backcheck",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning",
      summary_stat   = list(
        error_rate_by_variable   = dplyr::tibble(
          variable = character(), n_compared = integer(),
          n_error = integer(), error_rate = numeric()
        ),
        error_rate_by_enumerator = dplyr::tibble(
          enumerator = character(), n_compared = integer(),
          n_error = integer(), error_rate = numeric()
        )
      )
    ))
  }

  orig_matched <- data[match(matched_ids, orig_ids), , drop = FALSE]
  bc_matched   <- backcheck_data[match(matched_ids, bc_ids), , drop = FALSE]

  # Per-variable error rates
  per_var_errors <- vapply(t2_cols, function(col) {
    orig_vals <- as.character(orig_matched[[col]])
    bc_vals   <- as.character(bc_matched[[col]])
    sum(!(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals))),
        na.rm = FALSE)
  }, integer(1))

  error_rate_by_variable <- dplyr::tibble(
    variable   = t2_cols,
    n_compared = as.integer(length(matched_ids)),
    n_error    = as.integer(per_var_errors),
    error_rate = round(n_error / n_compared, 4)
  )

  # Per-enumerator error rates
  if (!is.null(enum_col)) {
    enums <- as_id(orig_matched[[enum_col]])

    # Build error matrix: rows = observations, cols = t2 variables
    error_matrix <- vapply(t2_cols, function(col) {
      orig_vals <- as.character(orig_matched[[col]])
      bc_vals   <- as.character(bc_matched[[col]])
      !(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals)))
    }, logical(length(matched_ids)))

    if (is.null(dim(error_matrix))) {
      total_errors <- as.integer(error_matrix)
    } else {
      total_errors <- rowSums(error_matrix)
    }

    n_t2 <- length(t2_cols)
    enum_error_df <- dplyr::tibble(
      enumerator   = enums,
      n_errors     = total_errors,
      n_cells      = as.integer(n_t2)
    ) |>
      dplyr::filter(!is.na(enumerator)) |>
      dplyr::group_by(enumerator) |>
      dplyr::summarise(
        n_compared = dplyr::n() * n_t2,
        n_error    = sum(n_errors),
        error_rate = round(n_error / n_compared, 4),
        .groups    = "drop"
      )

    flagged_enums <- enum_error_df |>
      dplyr::filter(error_rate > max_error_rate)
    flagged_ids <- as.character(flagged_enums$enumerator)
    flag_reason <- if (nrow(flagged_enums) > 0) {
      paste0("T2 error rate ", round(flagged_enums$error_rate * 100, 1),
             "% exceeds max ", max_error_rate * 100, "%")
    } else {
      character()
    }
  } else {
    enum_error_df <- dplyr::tibble(
      enumerator = character(), n_compared = integer(),
      n_error = integer(), error_rate = numeric()
    )
    flagged_ids <- character()
    flag_reason <- character()
  }

  new_check_result(
    check_name     = "J03_backcheck_t2_match",
    check_category = "backcheck",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(length(matched_ids)),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      error_rate_by_variable   = error_rate_by_variable,
      error_rate_by_enumerator = enum_error_df
    )
  )
}

#' Check Type 3 variable changes in back-checks
#'
#' J.04: Type 3 variables are expected to change between original and
#' back-check (e.g., satisfaction, attitude). This check is purely
#' informational and computes change rates per variable.
#'
#' @param data Data frame of original survey submissions
#' @param id_col Character. Primary key column in original data
#' @param backcheck_data Data frame of back-check submissions
#' @param bc_id_col Character. ID column in backcheck_data matching data
#' @param t3_cols Character vector. Type 3 variable names (must exist in both
#'   data and backcheck_data)
#' @param ... Reserved for future use
#' @return A check_result object (informational, n_flagged = 0)
#' @export
check_backcheck_t3_match <- function(data, id_col, backcheck_data, bc_id_col,
                                     t3_cols, ...) {
  assert_columns(data, c(id_col, t3_cols),
                 context = "check_backcheck_t3_match")
  assert_columns(backcheck_data, c(bc_id_col, t3_cols),
                 context = "check_backcheck_t3_match")

  orig_ids <- as_id(data[[id_col]])
  bc_ids   <- as_id(backcheck_data[[bc_id_col]])

  matched_ids <- intersect(
    orig_ids[!is.na(orig_ids)],
    bc_ids[!is.na(bc_ids)]
  )

  if (length(matched_ids) == 0L) {
    return(new_check_result(
      check_name     = "J04_backcheck_t3_match",
      check_category = "backcheck",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "info",
      summary_stat   = list(
        change_rate_by_variable = dplyr::tibble(
          variable = character(), n_compared = integer(),
          n_changed = integer(), change_rate = numeric()
        )
      )
    ))
  }

  orig_matched <- data[match(matched_ids, orig_ids), , drop = FALSE]
  bc_matched   <- backcheck_data[match(matched_ids, bc_ids), , drop = FALSE]

  per_var_changes <- vapply(t3_cols, function(col) {
    orig_vals <- as.character(orig_matched[[col]])
    bc_vals   <- as.character(bc_matched[[col]])
    sum(!(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals))),
        na.rm = FALSE)
  }, integer(1))

  change_rate_by_variable <- dplyr::tibble(
    variable    = t3_cols,
    n_compared  = as.integer(length(matched_ids)),
    n_changed   = as.integer(per_var_changes),
    change_rate = round(n_changed / n_compared, 4)
  )

  new_check_result(
    check_name     = "J04_backcheck_t3_match",
    check_category = "backcheck",
    n_flagged      = 0L,
    n_total        = as.integer(length(matched_ids)),
    flagged_ids    = character(),
    flag_reason    = character(),
    severity       = "info",
    summary_stat   = list(
      change_rate_by_variable = change_rate_by_variable
    )
  )
}

#' Check back-check error rates by enumerator
#'
#' J.06: Compute per-enumerator error rates across all comparison columns and
#' flag enumerators whose error rate is more than threshold_sd standard
#' deviations above the team mean.
#'
#' @param data Data frame of original survey submissions
#' @param id_col Character. Primary key column in original data
#' @param enum_col Character. Enumerator ID column in original data
#' @param backcheck_data Data frame of back-check submissions
#' @param bc_id_col Character. ID column in backcheck_data matching data
#' @param compare_cols Character vector. Columns to compare between original
#'   and back-check data (must exist in both)
#' @param threshold_sd Numeric. Number of SDs above team mean to trigger flag
#'   (default 2)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_backcheck_by_enumerator <- function(data, id_col, enum_col,
                                          backcheck_data, bc_id_col,
                                          compare_cols, threshold_sd = 2,
                                          ...) {
  assert_columns(data, c(id_col, enum_col, compare_cols),
                 context = "check_backcheck_by_enumerator")
  assert_columns(backcheck_data, c(bc_id_col, compare_cols),
                 context = "check_backcheck_by_enumerator")

  orig_ids <- as_id(data[[id_col]])
  bc_ids   <- as_id(backcheck_data[[bc_id_col]])

  matched_ids <- intersect(
    orig_ids[!is.na(orig_ids)],
    bc_ids[!is.na(bc_ids)]
  )

  if (length(matched_ids) == 0L) {
    return(new_check_result(
      check_name     = "J06_backcheck_by_enumerator",
      check_category = "backcheck",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning",
      summary_stat   = list(
        enumerator_error_rates = dplyr::tibble(
          enumerator = character(), n_surveys = integer(),
          n_cells = integer(), n_errors = integer(),
          error_rate = numeric(), z_score = numeric()
        ),
        team_mean = NA_real_,
        team_sd   = NA_real_
      )
    ))
  }

  orig_matched <- data[match(matched_ids, orig_ids), , drop = FALSE]
  bc_matched   <- backcheck_data[match(matched_ids, bc_ids), , drop = FALSE]
  enums        <- as_id(orig_matched[[enum_col]])

  # Build error matrix
  error_matrix <- vapply(compare_cols, function(col) {
    orig_vals <- as.character(orig_matched[[col]])
    bc_vals   <- as.character(bc_matched[[col]])
    !(orig_vals == bc_vals | (is.na(orig_vals) & is.na(bc_vals)))
  }, logical(length(matched_ids)))

  if (is.null(dim(error_matrix))) {
    row_errors <- as.integer(error_matrix)
  } else {
    row_errors <- rowSums(error_matrix)
  }

  n_compare <- length(compare_cols)

  enum_stats <- dplyr::tibble(
    enumerator = enums,
    n_errors   = row_errors
  ) |>
    dplyr::filter(!is.na(enumerator)) |>
    dplyr::group_by(enumerator) |>
    dplyr::summarise(
      n_surveys  = dplyr::n(),
      n_cells    = dplyr::n() * n_compare,
      n_errors   = sum(n_errors),
      error_rate = round(n_errors / n_cells, 4),
      .groups    = "drop"
    )

  team_mean <- mean(enum_stats$error_rate)
  team_sd   <- stats::sd(enum_stats$error_rate)

  if (is.na(team_sd) || team_sd == 0) {
    enum_stats$z_score <- 0
    flagged_ids <- character()
    flag_reason <- character()
  } else {
    enum_stats$z_score <- round(
      (enum_stats$error_rate - team_mean) / team_sd, 2
    )
    flagged <- enum_stats |> dplyr::filter(z_score > threshold_sd)
    flagged_ids <- as.character(flagged$enumerator)
    flag_reason <- if (nrow(flagged) > 0) {
      paste0("Error rate ", round(flagged$error_rate * 100, 1),
             "% (z=", flagged$z_score,
             ") vs team avg ", round(team_mean * 100, 1), "%")
    } else {
      character()
    }
  }

  new_check_result(
    check_name     = "J06_backcheck_by_enumerator",
    check_category = "backcheck",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(nrow(enum_stats)),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      enumerator_error_rates = enum_stats,
      team_mean              = team_mean,
      team_sd                = team_sd
    )
  )
}
