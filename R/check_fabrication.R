# ============================================================================
# check_fabrication.R — Data fabrication detection checks
# ============================================================================

#' Check Benford's Law first-digit distribution
#'
#' M.01: Apply Benford's Law first-digit test on a numeric column.
#' Computes the observed first-digit distribution (digits 1-9) and compares
#' to the expected Benford distribution using a chi-squared goodness-of-fit test.
#' Flags the VARIABLE (not individual records) if the distribution deviates
#' significantly from Benford's Law.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to test
#' @param alpha Numeric. Significance level for the chi-squared test (default 0.05)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_benford_first_digit <- function(data, id_col, num_col,
                                       alpha = 0.05, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_benford_first_digit")
  assert_numeric(data, num_col, context = "check_benford_first_digit")

  vals <- data[[num_col]]
  # Remove NAs and zeros; take absolute value for first digit extraction
  vals <- abs(vals[!is.na(vals) & vals != 0])
  n_values <- length(vals)

  # Extract first digit: strip to first non-zero digit
  first_digits <- as.integer(substr(format(vals, scientific = FALSE),
                                     regexpr("[1-9]", format(vals, scientific = FALSE)),
                                     regexpr("[1-9]", format(vals, scientific = FALSE))))
  # Safer extraction: use log10
  first_digits <- as.integer(substr(as.character(
    vals / 10^floor(log10(vals))
  ), 1, 1))
  # Ensure only digits 1-9

  first_digits <- first_digits[first_digits >= 1L & first_digits <= 9L &
                                 !is.na(first_digits)]
  n_values <- length(first_digits)

  # Benford expected probabilities for digits 1-9
  expected_prob <- log10(1 + 1 / (1:9))

  # Observed frequency table (ensure all digits 1-9 present)
  obs_table <- table(factor(first_digits, levels = 1:9))
  observed_freq <- as.integer(obs_table)
  expected_freq <- expected_prob * n_values

  # Chi-squared test (only if enough observations)
  if (n_values < 10) {
    chi_sq_stat <- NA_real_
    p_value <- NA_real_
    is_flagged <- FALSE
  } else {
    chi_test <- stats::chisq.test(observed_freq, p = expected_prob)
    chi_sq_stat <- unname(chi_test$statistic)
    p_value <- chi_test$p.value
    is_flagged <- p_value < alpha
  }

  flagged_ids <- if (is_flagged) num_col else character()
  flag_reason <- if (is_flagged) {
    paste0("First-digit distribution deviates from Benford's Law ",
           "(chi-sq = ", round(chi_sq_stat, 2),
           ", p = ", signif(p_value, 3), ")")
  } else character()

  new_check_result(
    check_name     = "M01_benford_first_digit",
    check_category = "fabrication",
    n_flagged      = length(flagged_ids),
    n_total        = 1L,
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable      = num_col,
      observed_freq = stats::setNames(observed_freq, paste0("d", 1:9)),
      expected_freq = stats::setNames(round(expected_freq, 2), paste0("d", 1:9)),
      chi_sq_stat   = chi_sq_stat,
      p_value       = p_value,
      n_values      = n_values
    )
  )
}

#' Check Benford's Law second-digit distribution
#'
#' M.02: Apply Benford's Law second-digit test on a numeric column.
#' Computes the observed second-digit distribution (digits 0-9) and compares
#' to the expected second-digit Benford distribution using a chi-squared test.
#'
#' Expected: P(d2) = sum over d1=1..9 of log10(1 + 1/(10*d1 + d2))
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to test
#' @param alpha Numeric. Significance level for the chi-squared test (default 0.05)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_benford_second_digit <- function(data, id_col, num_col,
                                        alpha = 0.05, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_benford_second_digit")
  assert_numeric(data, num_col, context = "check_benford_second_digit")

  vals <- data[[num_col]]
  # Remove NAs and zeros; need values with at least 2 digits
  vals <- abs(vals[!is.na(vals) & vals != 0])
  # Keep only values >= 10 (must have a second digit)
  vals <- vals[vals >= 10]
  n_values <- length(vals)

  # Extract second digit using integer arithmetic
  second_digits <- as.integer(floor(vals / 10^(floor(log10(vals)) - 1)) %% 10)
  second_digits <- second_digits[!is.na(second_digits) &
                                   second_digits >= 0L & second_digits <= 9L]
  n_values <- length(second_digits)

  # Expected probabilities for second digit (0-9)
  expected_prob <- vapply(0:9, function(d2) {
    sum(log10(1 + 1 / (10 * (1:9) + d2)))
  }, numeric(1))

  obs_table <- table(factor(second_digits, levels = 0:9))
  observed_freq <- as.integer(obs_table)
  expected_freq <- expected_prob * n_values

  if (n_values < 10) {
    chi_sq_stat <- NA_real_
    p_value <- NA_real_
    is_flagged <- FALSE
  } else {
    chi_test <- stats::chisq.test(observed_freq, p = expected_prob)
    chi_sq_stat <- unname(chi_test$statistic)
    p_value <- chi_test$p.value
    is_flagged <- p_value < alpha
  }

  flagged_ids <- if (is_flagged) num_col else character()
  flag_reason <- if (is_flagged) {
    paste0("Second-digit distribution deviates from Benford's Law ",
           "(chi-sq = ", round(chi_sq_stat, 2),
           ", p = ", signif(p_value, 3), ")")
  } else character()

  new_check_result(
    check_name     = "M02_benford_second_digit",
    check_category = "fabrication",
    n_flagged      = length(flagged_ids),
    n_total        = 1L,
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable      = num_col,
      observed_freq = stats::setNames(observed_freq, paste0("d", 0:9)),
      expected_freq = stats::setNames(round(expected_freq, 2), paste0("d", 0:9)),
      chi_sq_stat   = chi_sq_stat,
      p_value       = p_value,
      n_values      = n_values
    )
  )
}

#' Check terminal digit preference (digit heaping)
#'
#' M.03: Check the last (terminal) digit distribution of a numeric column.
#' Under no digit preference, terminal digits should be roughly uniformly
#' distributed across 0-9. Uses a chi-squared goodness-of-fit test against
#' the uniform distribution.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param num_col Character. Name of the numeric column to test
#' @param alpha Numeric. Significance level for the chi-squared test (default 0.05)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_digit_preference <- function(data, id_col, num_col,
                                    alpha = 0.05, ...) {
  assert_columns(data, c(id_col, num_col), context = "check_digit_preference")
  assert_numeric(data, num_col, context = "check_digit_preference")

  vals <- data[[num_col]]
  vals <- vals[!is.na(vals)]
  n_values <- length(vals)

  # Extract terminal (last) digit using modulo on absolute integer values
  terminal_digits <- abs(round(vals)) %% 10
  terminal_digits <- as.integer(terminal_digits)
  terminal_digits <- terminal_digits[!is.na(terminal_digits)]
  n_values <- length(terminal_digits)

  # Observed counts for digits 0-9
  obs_table <- table(factor(terminal_digits, levels = 0:9))
  digit_counts <- as.integer(obs_table)

  # Uniform expected probability
  expected_prob <- rep(0.1, 10)

  if (n_values < 10) {
    chi_sq_stat <- NA_real_
    p_value <- NA_real_
    is_flagged <- FALSE
  } else {
    chi_test <- stats::chisq.test(digit_counts, p = expected_prob)
    chi_sq_stat <- unname(chi_test$statistic)
    p_value <- chi_test$p.value
    is_flagged <- p_value < alpha
  }

  flagged_ids <- if (is_flagged) num_col else character()
  flag_reason <- if (is_flagged) {
    paste0("Terminal digit heaping detected ",
           "(chi-sq = ", round(chi_sq_stat, 2),
           ", p = ", signif(p_value, 3), ")")
  } else character()

  new_check_result(
    check_name     = "M03_digit_preference",
    check_category = "fabrication",
    n_flagged      = length(flagged_ids),
    n_total        = 1L,
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      variable    = num_col,
      digit_counts = stats::setNames(digit_counts, paste0("d", 0:9)),
      chi_sq_stat = chi_sq_stat,
      p_value     = p_value,
      n_values    = n_values
    )
  )
}

#' Check for straightlining (identical response patterns)
#'
#' B.09: For each survey, check what proportion of responses across check_cols
#' have the same value. Flag surveys where the proportion of identical
#' responses exceeds max_identical_rate.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param check_cols Character vector. Columns to inspect for straightlining
#' @param max_identical_rate Numeric. Maximum proportion of identical responses
#'   before flagging (default 0.8)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_straightlining <- function(data, id_col, check_cols,
                                  max_identical_rate = 0.8, ...) {
  assert_columns(data, c(id_col, check_cols), context = "check_straightlining")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)
  n_cols <- length(check_cols)

  # Compute identical-response rate per row
  identical_rates <- vapply(seq_len(n_total), function(i) {
    row_vals <- as.character(data[i, check_cols, drop = TRUE])
    # Remove NAs for the calculation
    row_vals <- row_vals[!is.na(row_vals)]
    if (length(row_vals) == 0) return(NA_real_)
    max(table(row_vals)) / length(row_vals)
  }, numeric(1))

  # Flag surveys exceeding threshold
  flagged_mask <- !is.na(identical_rates) & identical_rates > max_identical_rate
  flagged_ids <- ids[flagged_mask]
  flagged_rates <- identical_rates[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0(round(flagged_rates[i] * 100, 1),
           "% identical responses across ", n_cols,
           " columns (threshold: ", max_identical_rate * 100, "%)")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "B09_straightlining",
    check_category = "enumerator",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      identical_rates = stats::setNames(identical_rates, ids),
      n_cols_checked  = n_cols
    )
  )
}

#' Check response entropy (low diversity detection)
#'
#' M.06: For each survey, compute normalized Shannon entropy across check_cols.
#' Low entropy indicates suspiciously low response diversity, which may
#' indicate fabricated data.
#'
#' H = -sum(p * log2(p)) normalized by log2(n_categories).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param check_cols Character vector. Columns to compute entropy over
#' @param min_entropy_ratio Numeric. Minimum normalized entropy threshold;
#'   surveys below this are flagged (default 0.3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_response_entropy <- function(data, id_col, check_cols,
                                    min_entropy_ratio = 0.3, ...) {
  assert_columns(data, c(id_col, check_cols), context = "check_response_entropy")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  # Compute normalized Shannon entropy per row
  entropy_ratios <- vapply(seq_len(n_total), function(i) {
    row_vals <- as.character(data[i, check_cols, drop = TRUE])
    row_vals <- row_vals[!is.na(row_vals)]
    n <- length(row_vals)
    if (n <= 1) return(NA_real_)

    freq <- table(row_vals)
    p <- as.numeric(freq) / n
    # Shannon entropy in bits
    h <- -sum(p * log2(p))
    n_categories <- length(freq)
    if (n_categories <= 1) return(0)
    # Normalized entropy
    h / log2(n_categories)
  }, numeric(1))

  # Flag surveys with entropy below threshold
  flagged_mask <- !is.na(entropy_ratios) & entropy_ratios < min_entropy_ratio
  flagged_ids <- ids[flagged_mask]
  flagged_entropy <- entropy_ratios[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("Normalized entropy = ", round(flagged_entropy[i], 3),
           " (threshold: ", min_entropy_ratio, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "M06_response_entropy",
    check_category = "fabrication",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      entropy_ratios = stats::setNames(entropy_ratios, ids),
      n_cols_checked = length(check_cols)
    )
  )
}

#' Check for duplicate response patterns across surveys
#'
#' M.07: For each pair of surveys, compute the proportion of matching
#' responses across check_cols. Flag pairs exceeding similarity_threshold.
#' Uses fingerprint hashing for efficiency: only computes pairwise similarity
#' within groups sharing the same hash.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param check_cols Character vector. Columns to compare across surveys
#' @param similarity_threshold Numeric. Proportion of matching responses
#'   to trigger a flag (default 0.95)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_duplicate_response_patterns <- function(data, id_col, check_cols,
                                               similarity_threshold = 0.95,
                                               ...) {
  assert_columns(data, c(id_col, check_cols), context = "check_duplicate_response_patterns")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)
  n_cols <- length(check_cols)

  # Build fingerprint: paste all check_col values per row
  fingerprints <- apply(data[, check_cols, drop = FALSE], 1, function(row) {
    paste(as.character(row), collapse = "|")
  })

  # Group rows by fingerprint; exact matches are immediate flags
  fp_df <- data.frame(idx = seq_len(n_total), id = ids, fp = fingerprints,
                      stringsAsFactors = FALSE)
  fp_groups <- split(fp_df, fp_df$fp)

  flagged_pairs <- list()
  flagged_id_set <- character()

  for (grp in fp_groups) {
    if (nrow(grp) < 2) next

    # Within-group: exact fingerprint match means similarity = 1.0
    if (1.0 >= similarity_threshold) {
      # All pairs in this group are flagged
      grp_ids <- grp$id
      for (i in seq_len(nrow(grp) - 1)) {
        for (j in (i + 1):nrow(grp)) {
          flagged_pairs[[length(flagged_pairs) + 1]] <- list(
            id1 = grp_ids[i], id2 = grp_ids[j], similarity = 1.0
          )
        }
      }
      flagged_id_set <- c(flagged_id_set, grp_ids)
    }
  }

  # If threshold < 1.0, also check near-matches across all rows
  if (similarity_threshold < 1.0 && n_total <= 5000) {
    response_matrix <- as.matrix(data[, check_cols, drop = FALSE])
    for (i in seq_len(n_total - 1)) {
      for (j in (i + 1):n_total) {
        # Skip pairs already flagged as exact
        if (fingerprints[i] == fingerprints[j]) next
        n_match <- sum(response_matrix[i, ] == response_matrix[j, ],
                       na.rm = TRUE)
        # Count non-NA pairs for denominator
        n_comparable <- sum(!is.na(response_matrix[i, ]) &
                              !is.na(response_matrix[j, ]))
        if (n_comparable == 0) next
        sim <- n_match / n_comparable
        if (sim >= similarity_threshold) {
          flagged_pairs[[length(flagged_pairs) + 1]] <- list(
            id1 = ids[i], id2 = ids[j], similarity = round(sim, 4)
          )
          flagged_id_set <- c(flagged_id_set, ids[i], ids[j])
        }
      }
    }
  }

  flagged_id_set <- unique(flagged_id_set)

  flag_reason <- if (length(flagged_pairs) > 0) {
    vapply(flagged_pairs, function(p) {
      paste0("Pair (", p$id1, ", ", p$id2, ") similarity = ",
             round(p$similarity * 100, 1), "%")
    }, character(1))
  } else character()

  # Flatten flagged_ids: one entry per pair element
  # Use unique flagged IDs, with one reason per unique ID
  if (length(flagged_id_set) > 0) {
    # Build per-ID reason: list all pairs involving that ID
    per_id_reason <- vapply(flagged_id_set, function(fid) {
      involved <- Filter(function(p) p$id1 == fid || p$id2 == fid, flagged_pairs)
      partners <- vapply(involved, function(p) {
        other <- if (p$id1 == fid) p$id2 else p$id1
        paste0(other, " (", round(p$similarity * 100, 1), "%)")
      }, character(1))
      paste0("Near-duplicate with: ", paste(partners, collapse = ", "))
    }, character(1), USE.NAMES = FALSE)
  } else {
    per_id_reason <- character()
  }

  new_check_result(
    check_name     = "M07_duplicate_response_patterns",
    check_category = "fabrication",
    n_flagged      = length(flagged_id_set),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_id_set),
    flag_reason    = per_id_reason,
    severity       = "error",
    summary_stat   = list(
      n_pairs_flagged = length(flagged_pairs),
      pairs           = flagged_pairs,
      n_cols_compared = n_cols
    )
  )
}

#' Check intraclass correlation coefficient by enumerator
#'
#' M.04: Compute one-way ICC for each numeric variable, grouping by enumerator.
#' High ICC means enumerator identity explains a large share of variance,
#' which may indicate fabrication or systematic enumerator bias.
#'
#' ICC = (MSB - MSW) / (MSB + (n_avg - 1) * MSW)
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param num_cols Character vector. Numeric columns to compute ICC for
#' @param max_icc Numeric. Maximum acceptable ICC (default 0.15)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_icc_enumerator <- function(data, id_col, enum_col, num_cols,
                                  max_icc = 0.15, ...) {
  assert_columns(data, c(id_col, enum_col, num_cols),
                 context = "check_icc_enumerator")
  for (nc in num_cols) {
    assert_numeric(data, nc, context = "check_icc_enumerator")
  }

  enums <- as_id(data[[enum_col]])
  n_total <- length(num_cols)

  # Compute one-way ICC for each numeric column
  icc_results <- vapply(num_cols, function(nc) {
    vals <- data[[nc]]
    # Remove rows with NA in either enumerator or value
    valid <- !is.na(enums) & !is.na(vals)
    if (sum(valid) < 3) return(NA_real_)

    g <- factor(enums[valid])
    y <- vals[valid]

    # Need at least 2 groups with data
    grp_counts <- table(g)
    grp_counts <- grp_counts[grp_counts > 0]
    if (length(grp_counts) < 2) return(NA_real_)

    k <- length(grp_counts)       # number of groups
    n_total_obs <- sum(grp_counts) # total observations
    grand_mean <- mean(y)

    # Between-group sum of squares
    grp_means <- tapply(y, g, mean)
    grp_n <- tapply(y, g, length)
    ssb <- sum(grp_n * (grp_means - grand_mean)^2)
    msb <- ssb / (k - 1)

    # Within-group sum of squares
    ssw <- sum(tapply(y, g, function(x) sum((x - mean(x))^2)))
    msw <- ssw / (n_total_obs - k)

    # Average group size (harmonic mean for unbalanced designs)
    n_avg <- (n_total_obs - sum(grp_n^2) / n_total_obs) / (k - 1)

    # ICC(1) one-way random
    if ((msb + (n_avg - 1) * msw) == 0) return(NA_real_)
    icc_val <- (msb - msw) / (msb + (n_avg - 1) * msw)
    # Clamp to [-1, 1]
    max(min(icc_val, 1), -1)
  }, numeric(1))

  icc_by_variable <- dplyr::tibble(
    variable = num_cols,
    icc      = round(icc_results, 4)
  )

  # Flag variables where ICC > max_icc
  flagged_mask <- !is.na(icc_results) & icc_results > max_icc
  flagged_vars <- num_cols[flagged_mask]
  flagged_iccs <- icc_results[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_vars), function(i) {
    paste0("ICC = ", round(flagged_iccs[i], 4),
           " for '", flagged_vars[i], "' (max: ", max_icc, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "M04_icc_enumerator",
    check_category = "fabrication",
    n_flagged      = length(flagged_vars),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_vars),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      icc_by_variable = icc_by_variable,
      max_icc         = max_icc
    )
  )
}

#' Check variance ratio by enumerator
#'
#' M.08: For each numeric variable, compute the ratio of between-enumerator
#' variance to within-enumerator variance. A high ratio suggests enumerator
#' identity explains too much of the variability, possibly indicating
#' fabrication or systematic bias.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param num_cols Character vector. Numeric columns to test
#' @param max_ratio Numeric. Maximum acceptable between/within variance ratio
#'   (default 2)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_variance_ratio <- function(data, id_col, enum_col, num_cols,
                                  max_ratio = 2, ...) {
  assert_columns(data, c(id_col, enum_col, num_cols),
                 context = "check_variance_ratio")
  for (nc in num_cols) {
    assert_numeric(data, nc, context = "check_variance_ratio")
  }

  enums <- as_id(data[[enum_col]])
  n_total <- length(num_cols)

  # Compute between/within variance ratio per variable
  ratio_results <- vapply(num_cols, function(nc) {
    vals <- data[[nc]]
    valid <- !is.na(enums) & !is.na(vals)
    if (sum(valid) < 3) return(NA_real_)

    g <- factor(enums[valid])
    y <- vals[valid]

    grp_counts <- table(g)
    grp_counts <- grp_counts[grp_counts > 0]
    if (length(grp_counts) < 2) return(NA_real_)

    k <- length(grp_counts)
    n_total_obs <- sum(grp_counts)
    grand_mean <- mean(y)

    # Between-group mean squares
    grp_means <- tapply(y, g, mean)
    grp_n <- tapply(y, g, length)
    ssb <- sum(grp_n * (grp_means - grand_mean)^2)
    msb <- ssb / (k - 1)

    # Within-group mean squares
    ssw <- sum(tapply(y, g, function(x) sum((x - mean(x))^2)))
    df_within <- n_total_obs - k
    if (df_within <= 0) return(NA_real_)
    msw <- ssw / df_within

    if (msw == 0) return(NA_real_)
    msb / msw
  }, numeric(1))

  ratio_by_variable <- dplyr::tibble(
    variable       = num_cols,
    variance_ratio = round(ratio_results, 4)
  )

  # Flag variables where ratio > max_ratio
  flagged_mask <- !is.na(ratio_results) & ratio_results > max_ratio
  flagged_vars <- num_cols[flagged_mask]
  flagged_ratios <- ratio_results[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_vars), function(i) {
    paste0("Between/within variance ratio = ", round(flagged_ratios[i], 2),
           " for '", flagged_vars[i], "' (max: ", max_ratio, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "M08_variance_ratio",
    check_category = "fabrication",
    n_flagged      = length(flagged_vars),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_vars),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      ratio_by_variable = ratio_by_variable,
      max_ratio         = max_ratio
    )
  )
}
