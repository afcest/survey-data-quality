#' Apply corrections from a correction log to survey data
#'
#' Q.01: For each row in the correction log, find the matching record in data,
#' verify the old value matches, and apply the new value. Returns the corrected
#' data along with a summary of applied and skipped corrections.
#'
#' This is a data modification function, NOT a check function. It does not
#' return a check_result object.
#'
#' @param data Data frame to correct
#' @param correction_log Data frame with one row per correction. Must contain
#'   columns for record ID, variable name, old value, and new value.
#' @param id_col Character. Name of the ID column in both data and
#'   correction_log (default "id")
#' @param variable_col Character. Column in correction_log containing the
#'   variable name to correct (default "variable")
#' @param old_value_col Character. Column in correction_log containing the
#'   expected current value (default "old_value")
#' @param new_value_col Character. Column in correction_log containing the
#'   replacement value (default "new_value")
#' @param ... Reserved for future use
#' @return A list with components:
#'   \describe{
#'     \item{corrected_data}{The modified data frame}
#'     \item{n_applied}{Integer. Number of corrections successfully applied}
#'     \item{n_skipped}{Integer. Number of corrections that could not be applied}
#'     \item{skipped_log}{A tibble with columns: row_index, id, variable,
#'       reason (describing why the correction was skipped)}
#'   }
#' @export
apply_corrections <- function(data, correction_log, id_col = "id",
                              variable_col = "variable",
                              old_value_col = "old_value",
                              new_value_col = "new_value", ...) {
  assert_columns(data, id_col, context = "apply_corrections")
  assert_columns(correction_log, c(id_col, variable_col, old_value_col,
                                    new_value_col),
                 context = "apply_corrections")

  data_ids    <- as_id(data[[id_col]])
  n_applied   <- 0L
  n_skipped   <- 0L
  skipped_log <- dplyr::tibble(
    row_index = integer(),
    id        = character(),
    variable  = character(),
    reason    = character()
  )

  for (i in seq_len(nrow(correction_log))) {
    corr_id  <- as_id(correction_log[[id_col]][i])
    corr_var <- as.character(correction_log[[variable_col]][i])
    corr_old <- as.character(correction_log[[old_value_col]][i])
    corr_new <- as.character(correction_log[[new_value_col]][i])

    # Check: ID exists in data
    row_idx <- which(data_ids == corr_id)
    if (length(row_idx) == 0L) {
      n_skipped <- n_skipped + 1L
      skipped_log <- dplyr::bind_rows(skipped_log, dplyr::tibble(
        row_index = i, id = corr_id, variable = corr_var,
        reason = "ID not found in data"
      ))
      next
    }

    # Check: variable exists in data
    if (!corr_var %in% names(data)) {
      n_skipped <- n_skipped + 1L
      skipped_log <- dplyr::bind_rows(skipped_log, dplyr::tibble(
        row_index = i, id = corr_id, variable = corr_var,
        reason = paste0("Variable '", corr_var, "' not found in data")
      ))
      next
    }

    # Use first match if multiple rows share the same ID
    target_row <- row_idx[1L]
    current_val <- as.character(data[[corr_var]][target_row])

    # Check: old value matches current value
    both_na <- is.na(corr_old) & is.na(current_val)
    values_match <- both_na || (!is.na(corr_old) & !is.na(current_val) &
                                  corr_old == current_val)

    if (!values_match) {
      n_skipped <- n_skipped + 1L
      skipped_log <- dplyr::bind_rows(skipped_log, dplyr::tibble(
        row_index = i, id = corr_id, variable = corr_var,
        reason = paste0("Old value mismatch: expected '", corr_old,
                        "', found '", current_val, "'")
      ))
      next
    }

    # Apply correction, coercing type to match the target column
    if (is.numeric(data[[corr_var]])) {
      data[[corr_var]][target_row] <- as.numeric(corr_new)
    } else if (is.integer(data[[corr_var]])) {
      data[[corr_var]][target_row] <- as.integer(corr_new)
    } else if (is.logical(data[[corr_var]])) {
      data[[corr_var]][target_row] <- as.logical(corr_new)
    } else {
      data[[corr_var]][target_row] <- corr_new
    }

    n_applied <- n_applied + 1L
  }

  list(
    corrected_data = data,
    n_applied      = n_applied,
    n_skipped      = n_skipped,
    skipped_log    = skipped_log
  )
}

#' Validate a correction log before applying it
#'
#' Q.02: Check a correction log for potential issues without modifying data.
#' Flags entries where the ID does not exist in data, the variable does not
#' exist in data, or the old value does not match the current value.
#'
#' @param data Data frame to validate corrections against
#' @param correction_log Data frame with one row per correction
#' @param id_col Character. Name of the ID column (default "id")
#' @param variable_col Character. Column in correction_log containing the
#'   variable name (default "variable")
#' @param old_value_col Character. Column in correction_log containing the
#'   expected current value (default "old_value")
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_correction_log <- function(data, correction_log, id_col = "id",
                                 variable_col = "variable",
                                 old_value_col = "old_value", ...) {
  assert_columns(data, id_col, context = "check_correction_log")
  assert_columns(correction_log, c(id_col, variable_col, old_value_col),
                 context = "check_correction_log")

  data_ids   <- as_id(data[[id_col]])
  data_names <- names(data)
  n_total    <- nrow(correction_log)

  flagged_ids    <- character()
  flag_reason    <- character()
  n_invalid_id   <- 0L
  n_invalid_var  <- 0L
  n_val_mismatch <- 0L

  for (i in seq_len(n_total)) {
    corr_id  <- as_id(correction_log[[id_col]][i])
    corr_var <- as.character(correction_log[[variable_col]][i])
    corr_old <- as.character(correction_log[[old_value_col]][i])

    # Check 1: ID exists
    row_idx <- which(data_ids == corr_id)
    if (length(row_idx) == 0L) {
      n_invalid_id <- n_invalid_id + 1L
      flagged_ids  <- c(flagged_ids, paste0("row_", i))
      flag_reason  <- c(flag_reason,
                        paste0("ID '", corr_id, "' not found in data"))
      next
    }

    # Check 2: variable exists
    if (!corr_var %in% data_names) {
      n_invalid_var <- n_invalid_var + 1L
      flagged_ids   <- c(flagged_ids, paste0("row_", i))
      flag_reason   <- c(flag_reason,
                         paste0("Variable '", corr_var, "' not found in data"))
      next
    }

    # Check 3: old value matches current value
    target_row  <- row_idx[1L]
    current_val <- as.character(data[[corr_var]][target_row])
    both_na     <- is.na(corr_old) & is.na(current_val)
    values_match <- both_na || (!is.na(corr_old) & !is.na(current_val) &
                                  corr_old == current_val)

    if (!values_match) {
      n_val_mismatch <- n_val_mismatch + 1L
      flagged_ids    <- c(flagged_ids, paste0("row_", i))
      flag_reason    <- c(flag_reason,
                          paste0("Value mismatch for '", corr_var,
                                 "' on ID '", corr_id,
                                 "': expected '", corr_old,
                                 "', found '", current_val, "'"))
    }
  }

  n_valid <- n_total - length(flagged_ids)

  new_check_result(
    check_name     = "Q02_correction_log",
    check_category = "corrections",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_valid          = as.integer(n_valid),
      n_invalid_id     = as.integer(n_invalid_id),
      n_invalid_variable = as.integer(n_invalid_var),
      n_value_mismatch = as.integer(n_val_mismatch)
    )
  )
}

#' Recode "other specify" responses
#'
#' Q.03: Apply a recode map to free-text "other (specify)" responses, mapping
#' raw text values to standardized categories.
#'
#' This is a data modification function, NOT a check function. It does not
#' return a check_result object.
#'
#' @param data Data frame containing the other-specify column
#' @param id_col Character. Primary key column name (used for traceability)
#' @param other_col Character. Name of the other-specify column to recode
#' @param recode_map Named character vector. Names are the original text values
#'   (matched case-insensitively after trimming whitespace), values are the new
#'   categories. Example: \code{c("maiz" = "maize", "riz paddy" = "rice")}
#' @param target_col Character or NULL. If provided, write recoded values to
#'   this column (creating it if necessary). If NULL, modify other_col in place.
#' @param ... Reserved for future use
#' @return The modified data frame
#' @export
recode_other_specify <- function(data, id_col, other_col, recode_map,
                                 target_col = NULL, ...) {
  assert_columns(data, c(id_col, other_col),
                 context = "recode_other_specify")
  stopifnot(
    is.character(recode_map),
    !is.null(names(recode_map)),
    all(nchar(names(recode_map)) > 0)
  )

  # Normalize recode map keys: lowercase and trimmed
  norm_keys <- trimws(tolower(names(recode_map)))
  names(recode_map) <- norm_keys

  # Get source values, normalize for matching
  src_vals      <- as.character(data[[other_col]])
  src_norm      <- trimws(tolower(src_vals))
  recoded_vals  <- src_vals  # start with original values

  # Apply recode map
  match_idx <- match(src_norm, names(recode_map))
  matched   <- !is.na(match_idx)
  recoded_vals[matched] <- recode_map[match_idx[matched]]

  # Write to target column or in place
  out_col <- if (is.null(target_col)) other_col else target_col
  data[[out_col]] <- recoded_vals

  data
}
