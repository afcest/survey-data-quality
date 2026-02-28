#' Read and validate a project configuration file
#'
#' @param path Path to YAML config file
#' @return A validated config list of class "adc_config"
#' @export
read_config <- function(path) {

  if (!file.exists(path)) cli::cli_abort("Config file not found: {path}")
  cfg <- yaml::read_yaml(path)
  validate_config(cfg)
  structure(cfg, class = "adc_config")
}

#' Validate config structure
#' @param cfg List from YAML
#' @keywords internal
validate_config <- function(cfg) {
  required_sections <- c("project", "variables")
  missing <- setdiff(required_sections, names(cfg))
  if (length(missing) > 0) {
    cli::cli_abort("Config missing required sections: {paste(missing, collapse = ', ')}")
  }

  required_vars <- c("id", "enumerator")
  missing_vars <- setdiff(required_vars, names(cfg$variables))
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      "Config$variables missing required fields: {paste(missing_vars, collapse = ', ')}"
    )
  }
  invisible(cfg)
}

#' Get a config value with a default fallback
#'
#' @param cfg adc_config object
#' @param ... Path components (e.g., "checks", "outliers", "method")
#' @param default Default value if path not found
#' @return The config value or default
#' @export
cfg_get <- function(cfg, ..., default = NULL) {
  path <- c(...)
  val <- cfg
  for (key in path) {
    if (is.null(val[[key]])) return(default)
    val <- val[[key]]
  }
  val
}
