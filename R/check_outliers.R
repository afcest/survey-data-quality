#' Check outliers using IQR method
#'
#' D.01: Flag values below Q1 - multiplier*IQR or above Q3 + multiplier*IQR.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to check
#' @param multiplier Numeric. IQR multiplier for fence calculation (default 1.5)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_outliers_iqr <- function(data, id_col, num_col, multiplier = 1.5, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_outliers_iqr")
  assert_numeric(data, num_col, context = "check_outliers_iqr")

  ids <- as_id(data[[id_col]])
  vals <- data[[num_col]]
  n_total <- sum(!is.na(vals))

  # Compute IQR bounds on non-NA values
  q1 <- stats::quantile(vals, 0.25, na.rm = TRUE, names = FALSE)
  q3 <- stats::quantile(vals, 0.75, na.rm = TRUE, names = FALSE)
  iqr_val <- q3 - q1
  lower_bound <- q1 - multiplier * iqr_val
  upper_bound <- q3 + multiplier * iqr_val

  # Flag non-NA values outside the fences
  below <- !is.na(vals) & vals < lower_bound
  above <- !is.na(vals) & vals > upper_bound
  outlier_mask <- below | above

  flagged_ids <- ids[outlier_mask]
  flagged_vals <- vals[outlier_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    v <- flagged_vals[i]
    if (v < lower_bound) {
      paste0(num_col, " = ", v, " below lower bound ", round(lower_bound, 2))
    } else {
      paste0(num_col, " = ", v, " above upper bound ", round(upper_bound, 2))
    }
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "D01_outlier_iqr",
    check_category = "outliers",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable    = num_col,
      Q1          = q1,
      Q3          = q3,
      IQR         = iqr_val,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      n_below     = sum(below),
      n_above     = sum(above)
    )
  )
}

#' Check outliers using robust Z-score
#'
#' D.02: Flag values with absolute Z-score exceeding a threshold.
#' Uses robust statistics: median and MAD instead of mean and SD.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to check
#' @param threshold Numeric. Z-score threshold for flagging (default 3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_outliers_zscore <- function(data, id_col, num_col, threshold = 3, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_outliers_zscore")
  assert_numeric(data, num_col, context = "check_outliers_zscore")

  ids <- as_id(data[[id_col]])
  vals <- data[[num_col]]
  n_total <- sum(!is.na(vals))

  med <- stats::median(vals, na.rm = TRUE)
  mad_val <- stats::mad(vals, na.rm = TRUE)

  # Compute robust z-scores; if MAD is 0, no outliers can be detected
  if (mad_val == 0) {
    flagged_ids <- character()
    flag_reason <- character()
  } else {
    z_scores <- (vals - med) / mad_val
    outlier_mask <- !is.na(z_scores) & abs(z_scores) > threshold

    flagged_ids <- ids[outlier_mask]
    flagged_vals <- vals[outlier_mask]
    flagged_z <- z_scores[outlier_mask]

    flag_reason <- vapply(seq_along(flagged_ids), function(i) {
      paste0(num_col, " = ", flagged_vals[i],
             " (z-score = ", round(flagged_z[i], 2), ")")
    }, character(1), USE.NAMES = FALSE)
  }

  new_check_result(
    check_name     = "D02_outlier_zscore",
    check_category = "outliers",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable  = num_col,
      median    = med,
      mad       = mad_val,
      threshold = threshold
    )
  )
}

#' Check outliers using Modified Z-score (MAD method)
#'
#' D.03: Flag values with absolute modified Z-score exceeding a threshold.
#' Modified Z-score: M_i = 0.6745 * (x_i - median) / MAD.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to check
#' @param threshold Numeric. Modified Z-score threshold for flagging (default 3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_outliers_mad <- function(data, id_col, num_col, threshold = 3, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_outliers_mad")
  assert_numeric(data, num_col, context = "check_outliers_mad")

  ids <- as_id(data[[id_col]])
  vals <- data[[num_col]]
  n_total <- sum(!is.na(vals))

  med <- stats::median(vals, na.rm = TRUE)
  mad_val <- stats::mad(vals, constant = 1, na.rm = TRUE)

  # Compute modified z-scores; if MAD is 0, no outliers can be detected
  if (mad_val == 0) {
    flagged_ids <- character()
    flag_reason <- character()
  } else {
    m_scores <- 0.6745 * (vals - med) / mad_val
    outlier_mask <- !is.na(m_scores) & abs(m_scores) > threshold

    flagged_ids <- ids[outlier_mask]
    flagged_vals <- vals[outlier_mask]
    flagged_m <- m_scores[outlier_mask]

    flag_reason <- vapply(seq_along(flagged_ids), function(i) {
      paste0(num_col, " = ", flagged_vals[i],
             " (modified z-score = ", round(flagged_m[i], 2), ")")
    }, character(1), USE.NAMES = FALSE)
  }

  new_check_result(
    check_name     = "D03_outlier_mad",
    check_category = "outliers",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable  = num_col,
      median    = med,
      mad       = mad_val,
      threshold = threshold
    )
  )
}

#' Check values against hard range constraints
#'
#' D.04: Flag values outside a fixed \[min_val, max_val\] range.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to check
#' @param min_val Numeric. Minimum allowed value (default -Inf)
#' @param max_val Numeric. Maximum allowed value (default Inf)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_hard_range <- function(data, id_col, num_col, min_val = -Inf,
                             max_val = Inf, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_hard_range")
  assert_numeric(data, num_col, context = "check_hard_range")

  ids <- as_id(data[[id_col]])
  vals <- data[[num_col]]
  n_total <- sum(!is.na(vals))

  below <- !is.na(vals) & vals < min_val
  above <- !is.na(vals) & vals > max_val
  outlier_mask <- below | above

  flagged_ids <- ids[outlier_mask]
  flagged_vals <- vals[outlier_mask]
  flagged_below <- below[outlier_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    v <- flagged_vals[i]
    if (flagged_below[i]) {
      paste0(num_col, " = ", v, " below minimum ", min_val)
    } else {
      paste0(num_col, " = ", v, " above maximum ", max_val)
    }
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "D04_hard_range",
    check_category = "outliers",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      variable   = num_col,
      min_val    = min_val,
      max_val    = max_val,
      actual_min = min(vals, na.rm = TRUE),
      actual_max = max(vals, na.rm = TRUE)
    )
  )
}

#' Check for negative values
#'
#' D.09: Flag negative values in columns that should be non-negative
#' (e.g., counts, areas, quantities).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to check
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_negative_values <- function(data, id_col, num_col, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_negative_values")
  assert_numeric(data, num_col, context = "check_negative_values")

  ids <- as_id(data[[id_col]])
  vals <- data[[num_col]]
  n_total <- sum(!is.na(vals))

  neg_mask <- !is.na(vals) & vals < 0
  flagged_ids <- ids[neg_mask]
  flagged_vals <- vals[neg_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0(num_col, " = ", flagged_vals[i], " is negative")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "D09_negative_value",
    check_category = "outliers",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable   = num_col,
      n_negative = sum(neg_mask),
      min_value  = if (any(neg_mask)) min(flagged_vals) else NA_real_
    )
  )
}
