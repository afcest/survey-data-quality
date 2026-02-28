#' Check missing rate by variable
#'
#' C.01: Compute missing rate per variable and flag those above threshold.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param threshold Numeric. Missing rate threshold (default 0.05 = 5%)
#' @param exclude_cols Character vector of columns to exclude from check
#' @param ... Reserved
#' @return A check_result object
#' @export
check_missing_by_variable <- function(data, id_col, threshold = 0.05,
                                       exclude_cols = NULL, ...) {
  assert_columns(data, id_col, context = "check_missing_by_variable")

  check_cols <- setdiff(names(data), c(id_col, exclude_cols))
  n_total <- nrow(data)

  miss_stats <- dplyr::tibble(
    variable   = check_cols,
    n_missing  = vapply(check_cols, function(col) sum(is.na(data[[col]])), numeric(1)),
    n_total    = as.integer(n_total),
    miss_rate  = n_missing / n_total
  )

  flagged <- miss_stats |> dplyr::filter(miss_rate > threshold)

  # Determine severity: warning if 5-20%, error if >20%
  max_rate <- if (nrow(flagged) > 0) max(flagged$miss_rate) else 0
  sev <- if (max_rate > 0.20) "error" else "warning"

  # Guard: when no rows flagged, use empty character vectors explicitly
  if (nrow(flagged) == 0L) {
    f_ids <- character()
    f_reasons <- character()
  } else {
    f_ids <- flagged$variable
    f_reasons <- paste0(
      round(flagged$miss_rate * 100, 1), "% missing (",
      flagged$n_missing, "/", flagged$n_total, ")"
    )
  }

  new_check_result(
    check_name     = "C01_missing_by_variable",
    check_category = "completeness",
    n_flagged      = nrow(flagged),
    n_total        = as.integer(length(check_cols)),
    flagged_ids    = f_ids,
    flag_reason    = f_reasons,
    severity       = sev,
    summary_stat   = list(miss_stats = miss_stats)
  )
}

#' Check missing rate by enumerator
#'
#' C.02: Flag enumerators with unusually high missing rates.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param enum_col Enumerator ID column
#' @param threshold_sd Numeric. Number of SD above team mean to flag (default 2)
#' @param ... Reserved
#' @return A check_result object
#' @export
check_missing_by_enumerator <- function(data, id_col, enum_col,
                                         threshold_sd = 2, ...) {
  assert_columns(data, c(id_col, enum_col), context = "check_missing_by_enumerator")

  # Compute per-enumerator mean missing rate across all columns
  check_cols <- setdiff(names(data), c(id_col, enum_col))
  n_total_cols <- length(check_cols)

  enum_miss <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(enum_col))) |>
    dplyr::summarise(
      n_surveys = dplyr::n(),
      mean_miss_rate = mean(rowSums(is.na(dplyr::pick(dplyr::all_of(check_cols)))) /
                              n_total_cols),
      .groups = "drop"
    )

  team_mean <- mean(enum_miss$mean_miss_rate)
  team_sd <- stats::sd(enum_miss$mean_miss_rate)

  if (is.na(team_sd) || team_sd == 0) {
    flagged <- dplyr::tibble()
  } else {
    enum_miss$z_score <- (enum_miss$mean_miss_rate - team_mean) / team_sd
    flagged <- enum_miss |> dplyr::filter(z_score > threshold_sd)
  }

  new_check_result(
    check_name     = "C02_missing_by_enumerator",
    check_category = "completeness",
    n_flagged      = nrow(flagged),
    n_total        = nrow(enum_miss),
    flagged_ids    = as_id(flagged[[enum_col]]),
    flag_reason    = if (nrow(flagged) > 0) {
      paste0(
        round(flagged$mean_miss_rate * 100, 1),
        "% mean missing rate (team avg: ",
        round(team_mean * 100, 1), "%)"
      )
    } else character(),
    severity       = "warning",
    summary_stat   = list(
      enum_stats = enum_miss,
      team_mean = team_mean,
      team_sd = team_sd,
      threshold_sd = threshold_sd
    )
  )
}

#' Check for variables with 100% missing values
#'
#' C.04: Variables with zero non-missing values.
#'
#' @param data Data frame
#' @param ... Reserved
#' @return A check_result object
#' @export
check_all_missing_variables <- function(data, ...) {
  all_miss <- vapply(data, function(col) all(is.na(col)), logical(1))
  flagged_vars <- names(all_miss[all_miss])

  new_check_result(
    check_name     = "C04_all_missing_variable",
    check_category = "completeness",
    n_flagged      = length(flagged_vars),
    n_total        = as.integer(ncol(data)),
    flagged_ids    = flagged_vars,
    flag_reason    = rep("100% missing values", length(flagged_vars)),
    severity       = "error"
  )
}

#' Check skip pattern consistency
#'
#' C.03: Verify that conditional questions follow skip logic.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param parent_col Parent (trigger) column name
#' @param child_col Child (conditional) column name
#' @param parent_value Value of parent that triggers the skip
#' @param expect_child_na Logical. If TRUE, child should be NA when parent == parent_value
#' @param ... Reserved
#' @return A check_result object
#' @export
check_skip_pattern <- function(data, id_col, parent_col, child_col,
                                parent_value, expect_child_na = TRUE, ...) {
  assert_columns(data, c(id_col, parent_col, child_col),
                 context = "check_skip_pattern")

  ids <- as_id(data[[id_col]])

  if (expect_child_na) {
    # When parent == value, child SHOULD be NA
    violations <- which(data[[parent_col]] == parent_value & !is.na(data[[child_col]]))
  } else {
    # When parent == value, child should NOT be NA
    violations <- which(data[[parent_col]] == parent_value & is.na(data[[child_col]]))
  }

  direction <- if (expect_child_na) "should be empty" else "should not be empty"

  new_check_result(
    check_name     = "C03_skip_pattern",
    check_category = "completeness",
    n_flagged      = length(violations),
    n_total        = as.integer(sum(data[[parent_col]] == parent_value, na.rm = TRUE)),
    flagged_ids    = ids[violations],
    flag_reason    = rep(
      paste0(child_col, " ", direction, " when ", parent_col, " == ", parent_value),
      length(violations)
    ),
    severity       = "warning",
    summary_stat   = list(
      parent_col = parent_col, child_col = child_col,
      parent_value = parent_value, expect_child_na = expect_child_na
    )
  )
}
