#' Create a standardized check result
#'
#' Every check function in afcestDataCheck returns this structure.
#'
#' @param check_name Character. Unique check identifier (e.g., "A01_duplicate_id")
#' @param check_category Character. Category (e.g., "identification", "enumerator")
#' @param n_flagged Integer. Number of flagged records
#' @param n_total Integer. Total records examined
#' @param flagged_ids Character vector. IDs of flagged records
#' @param flag_reason Character vector. Reason per flagged ID
#' @param severity Character. One of "error", "warning", "info"
#' @param summary_stat List. Check-specific summary statistics
#' @return A list of class "check_result"
#' @export
new_check_result <- function(check_name,
                              check_category,
                              n_flagged = 0L,
                              n_total = 0L,
                              flagged_ids = character(),
                              flag_reason = character(),
                              severity = c("error", "warning", "info"),
                              summary_stat = list()) {
  severity <- match.arg(severity)
  stopifnot(
    is.character(check_name), length(check_name) == 1L,
    is.character(check_category), length(check_category) == 1L,
    is.character(flagged_ids),
    is.character(flag_reason),
    length(flagged_ids) == length(flag_reason) || length(flag_reason) == 0L,
    is.list(summary_stat)
  )

  structure(
    list(
      check_name     = check_name,
      check_category = check_category,
      n_flagged      = as.integer(n_flagged),
      n_total        = as.integer(n_total),
      flagged_ids    = flagged_ids,
      flag_reason    = flag_reason,
      severity       = severity,
      summary_stat   = summary_stat,
      timestamp      = Sys.time()
    ),
    class = "check_result"
  )
}

#' Print a check result
#' @param x A check_result object
#' @param ... Ignored
#' @export
print.check_result <- function(x, ...) {
  icon <- switch(x$severity, error = "x", warning = "!", info = "i")
  cat(paste0(
    "[", icon, "] ", x$check_name, ": ",
    x$n_flagged, "/", x$n_total, " flagged (", x$severity, ")\n"
  ))
  invisible(x)
}

#' Test if an object is a check_result
#' @param x Object to test
#' @return Logical
#' @export
is_check_result <- function(x) inherits(x, "check_result")

#' Combine multiple check results into a summary tibble
#' @param ... check_result objects or lists of check_result objects
#' @return A tibble summarizing all checks
#' @export
bind_check_results <- function(...) {
  results <- list(...)
  # Recursively flatten nested lists while preserving check_result objects
  flatten_results <- function(x) {
    if (is_check_result(x)) return(list(x))
    if (is.list(x)) return(unlist(lapply(x, flatten_results), recursive = FALSE))
    list()
  }
  results <- flatten_results(results)

  if (length(results) == 0L) {
    return(dplyr::tibble(
      check_name = character(), check_category = character(),
      n_flagged = integer(), n_total = integer(),
      pct_flagged = numeric(), severity = character()
    ))
  }

  dplyr::tibble(
    check_name     = vapply(results, `[[`, character(1), "check_name"),
    check_category = vapply(results, `[[`, character(1), "check_category"),
    n_flagged      = vapply(results, `[[`, integer(1), "n_flagged"),
    n_total        = vapply(results, `[[`, integer(1), "n_total"),
    pct_flagged    = ifelse(n_total > 0, round(n_flagged / n_total * 100, 2), 0),
    severity       = vapply(results, `[[`, character(1), "severity")
  )
}
