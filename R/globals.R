# Global variable bindings to suppress R CMD CHECK NOTEs
utils::globalVariables(c(
  "n_total", "n_flagged", "enum", "dk_count", "total_dk", "total_cells",
  "z_score", ".fp", ".id", "duration", "n_surveys", "start_time",
  "gap_minutes", "n_missing", "miss_rate", ".data", "target", "actual",
  "enumerator", "n_errors", "n_cells", "id", "stratum", "n_matched",
  "n_compared", "n_error", "error_rate", "n_changed"
))
