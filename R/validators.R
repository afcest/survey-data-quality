#' Validate that a data frame has required columns
#' @param data Data frame
#' @param required Character vector of required column names
#' @param context Character describing the calling context
#' @keywords internal
assert_columns <- function(data, required, context = "check") {
  stopifnot(is.data.frame(data))
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required columns for {context}:",
      "x" = "Not found: {paste(missing, collapse = ', ')}",
      "i" = "Available: {paste(head(names(data), 10), collapse = ', ')}{if (ncol(data) > 10) '...' else ''}"
    ))
  }
  invisible(TRUE)
}

#' Validate that a column is numeric
#' @param data Data frame
#' @param col Column name
#' @param context Calling context
#' @keywords internal
assert_numeric <- function(data, col, context = "check") {
  if (!is.numeric(data[[col]])) {
    cli::cli_abort(
      "Column '{col}' must be numeric for {context}, got {class(data[[col]])[1]}"
    )
  }
  invisible(TRUE)
}

#' Safely coerce to ID character vector
#' @param x Vector to coerce
#' @return Character vector
#' @keywords internal
as_id <- function(x) as.character(x)
