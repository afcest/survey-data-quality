#' Sample household survey data with intentional quality issues
#'
#' A realistic household survey dataset based on the World Bank's Living with
#' HIV Follow-Up 2 survey (Rwanda, 2023) from the \code{iehfc} R package.
#' Contains 1206 observations with real survey structure augmented by synthetic
#' columns (GPS, timing, names, phones) and deliberate data quality problems
#' planted in the last rows, making it ideal for demonstrating and testing
#' \code{afcestDataCheck} functions.
#'
#' Column names are aligned with the package's
#' \code{inst/example_config.yml} so that users can run
#' \code{run_all_checks(survey_sample, config)} out of the box.
#'
#' @section Planted quality issues:
#' \describe{
#'   \item{Duplicates}{3 natural duplicate hh_ids from the original data.}
#'   \item{GPS null island}{Row 1100 has lat=0, lon=0.}
#'   \item{GPS swap}{Row 1101 has latitude and longitude swapped (30.55, -1.85).}
#'   \item{GPS accuracy}{Row 1102 has accuracy of 200 m.}
#'   \item{Duration}{Row 1103 is 2 minutes (too short); row 1104 is 300 minutes (too long).}
#'   \item{Future date}{Row 1105 has a submission date in the future.}
#'   \item{Consent}{Rows 1106-1107 have consent = "no".}
#'   \item{Household size}{Row 1108 has hh_size = 55 (implausible).}
#'   \item{Name}{Row 1109 is "test"; row 1110 is a single character "X".}
#'   \item{Phone}{Row 1111 has an invalid phone number ("12").}
#'   \item{Sentinel values}{Real data contains -88/-66 sentinel values in income fields.}
#'   \item{Form version}{Real data has 2 form versions ("v1" and "v2").}
#'   \item{Comments}{3 planted field comments in mostly-NA column.}
#' }
#'
#' @format A data frame with 1206 rows and 28 variables:
#' \describe{
#'   \item{hh_id}{Character. Household identifier from original hhid (real data). Has 3 natural duplicates.}
#'   \item{enum_id}{Character. Enumerator ID formatted as ENUM + 3-digit number (e.g., ENUM046) from original enumerator field (real).}
#'   \item{submission_date}{Date. Survey submission date (real, from submissiondate, range ~May 2023).}
#'   \item{start}{POSIXct. Interview start timestamp (synthetic, realistic times 7AM-4PM).}
#'   \item{end}{POSIXct. Interview end timestamp (synthetic, start + duration).}
#'   \item{duration}{Numeric. Interview duration in minutes (synthetic, lognormal ~40min median).}
#'   \item{gps_lat}{Numeric. GPS latitude (synthetic, centered on Kayonza -1.85 / Rwamagana -1.95).}
#'   \item{gps_lon}{Numeric. GPS longitude (synthetic, centered on Kayonza 30.55 / Rwamagana 30.43).}
#'   \item{gps_accuracy}{Numeric. GPS accuracy in meters (synthetic, 3-15m range).}
#'   \item{consent}{Character. Whether respondent consented ("yes"/"no"; synthetic, almost all "yes").}
#'   \item{form_version}{Character. Survey form version ("v1" or "v2") from real formdef_version field.}
#'   \item{hh_size}{Integer. Number of household members (synthetic, Poisson(4)+1).}
#'   \item{income}{Numeric. Annual income from real inc_1 field.}
#'   \item{income_crop}{Integer. Crop income from real inc_2 field.}
#'   \item{expenditure}{Integer. Expenditure from real exp_25_1 field.}
#'   \item{crop_type}{Character. Primary crop type from real a_crop_c1_p1.}
#'   \item{crop_other}{Character. Other crop specify from real a_crop_c1_p1_other.}
#'   \item{yield_kg}{Numeric. Crop yield in kg from real crp08qa_c1_p1.}
#'   \item{crop_sold_qty}{Numeric. Quantity sold from real crp09qa_c1_p1.}
#'   \item{crop_revenue}{Integer. Crop revenue from real crp10a_c1_p1.}
#'   \item{province}{Character. Province (real).}
#'   \item{district}{Character. District (real, Kayonza or Rwamagana).}
#'   \item{sector}{Character. Sector (real).}
#'   \item{village}{Character. Village (real).}
#'   \item{name_head}{Character. Household head name (synthetic, Rwandan names).}
#'   \item{phone}{Character. Phone number (synthetic, Rwanda format 07X XXX XXX).}
#'   \item{device_id}{Character. Device ID (real).}
#'   \item{comments}{Character. Field comments (mostly NA, 3 planted).}
#' }
#'
#' @source Based on real survey data from the World Bank's \code{iehfc} R package
#'   (Living with HIV Follow-Up 2 survey, Rwanda, 2023). Augmented with
#'   synthetic columns for GPS, timing, consent, household size, names, and
#'   phone numbers. See \code{data-raw/survey_sample.R} for the creation script.
#' @examples
#' data(survey_sample)
#' str(survey_sample)
#'
#' # Run duplicate check
#' check_duplicate_ids(survey_sample, id_col = "hh_id")
#'
#' # Run duration check
#' check_survey_duration(survey_sample, id_col = "hh_id",
#'                       duration_col = "duration")
"survey_sample"


#' Sample back-check data for reconciliation checks
#'
#' A dataset of 20 back-check interviews matching records in
#' \code{\link{survey_sample}}. Contains intentional mismatches across
#' Type 1 (exact match), Type 2 (categorical), and Type 3 (continuous)
#' variables, suitable for testing the back-check reconciliation functions.
#'
#' @section Planted mismatches:
#' \describe{
#'   \item{Type 1 (consent)}{Row 3 has consent = "no" vs. "yes" in original.}
#'   \item{Type 2 (hh_size)}{Rows 5, 8, and 12 differ by 1-2 members.}
#'   \item{Type 3 (income)}{Rows 2, 7, and 15 have large differences; row 10 has a small difference.}
#'   \item{Yield}{Rows 6 and 14 differ from original.}
#' }
#'
#' @format A data frame with 20 rows and 8 variables:
#' \describe{
#'   \item{bc_id}{Character. Back-check interview identifier (BC_001 to BC_020).}
#'   \item{hh_id}{Character. Household identifier matching \code{survey_sample}.}
#'   \item{bc_enum_id}{Character. Back-check enumerator identifier (BC_ENUM01/02/03).}
#'   \item{consent}{Character. Consent response ("yes"/"no").}
#'   \item{hh_size}{Integer. Reported household size.}
#'   \item{income}{Numeric. Reported annual income.}
#'   \item{crop_type}{Character. Reported primary crop type.}
#'   \item{yield_kg}{Numeric. Reported crop yield in kg.}
#' }
#'
#' @source Based on real survey data from the World Bank's \code{iehfc} R package
#'   (Living with HIV Follow-Up 2 survey, Rwanda, 2023).
#'   See \code{data-raw/survey_sample.R} for the creation script.
#' @examples
#' data(survey_sample)
#' data(backcheck_sample)
#'
#' # Check back-check coverage
#' check_backcheck_coverage(
#'   data = survey_sample,
#'   id_col = "hh_id",
#'   backcheck_data = backcheck_sample,
#'   bc_id_col = "hh_id"
#' )
"backcheck_sample"


#' Sample correction log
#'
#' A small correction log with 5 entries that correct known quality issues
#' in \code{\link{survey_sample}}. Demonstrates the expected format for
#' \code{\link{apply_corrections}}.
#'
#' @format A data frame with 5 rows and 5 variables:
#' \describe{
#'   \item{hh_id}{Character. Household identifier matching \code{survey_sample}.}
#'   \item{variable}{Character. Name of the variable to correct.}
#'   \item{old_value}{Character. Current (incorrect) value as a string.}
#'   \item{new_value}{Character. Corrected value as a string.}
#'   \item{reason}{Character. Explanation for the correction.}
#' }
#'
#' @source Based on real survey data from the World Bank's \code{iehfc} R package
#'   (Living with HIV Follow-Up 2 survey, Rwanda, 2023).
#'   See \code{data-raw/survey_sample.R} for the creation script.
#' @examples
#' data(survey_sample)
#' data(corrections_sample)
#'
#' # Preview corrections
#' corrections_sample
#'
#' # Apply corrections
#' result <- apply_corrections(
#'   data = survey_sample,
#'   correction_log = corrections_sample,
#'   id_col = "hh_id"
#' )
#' result$n_applied
"corrections_sample"
