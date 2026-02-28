#' Sample household survey data with intentional quality issues
#'
#' A synthetic but realistic household survey dataset modeled on agricultural
#' surveys in Burkina Faso. Contains 100 observations with deliberate data
#' quality problems planted across all check categories, making it ideal for
#' demonstrating and testing \code{afcestDataCheck} functions.
#'
#' Column names are aligned with the package's
#' \code{inst/example_config.yml} so that users can run
#' \code{run_all_checks(survey_sample, config)} out of the box.
#'
#' @section Planted quality issues:
#' \describe{
#'   \item{Duplicates}{Rows 98-100 duplicate hh_ids of rows 1-3.}
#'   \item{Future date}{Row 95 has a submission date 30 days in the future.}
#'   \item{Outside collection window}{Row 96 is dated 2024-12-01 (before survey start).}
#'   \item{Duration}{Row 90 is 3 minutes (too short); row 91 is 210 minutes (too long).}
#'   \item{GPS null island}{Row 85 has lat=0, lon=0.}
#'   \item{GPS swap}{Row 86 has latitude and longitude swapped.}
#'   \item{GPS accuracy}{Row 87 has accuracy of 150 m.}
#'   \item{Consent}{Rows 92-93 have consent = "no".}
#'   \item{Form version}{Rows 88-89 use "v2" instead of "v1".}
#'   \item{Household size}{Row 80 has hh_size = 50 (implausible).}
#'   \item{Income}{Row 75 is negative; rows 76-77 are extreme outliers (>5M CFA).}
#'   \item{Expenditure}{Row 78 has expenditure > 4x income.}
#'   \item{Age}{Row 82 has age_head = 3 (implausible).}
#'   \item{Name}{Row 83 is "test"; row 84 is a single character "A".}
#'   \item{Phone}{Row 81 has a 3-digit phone number.}
#'   \item{Yield}{Rows 73-74 are extreme outliers (>15000 kg).}
#'   \item{Other specify}{Rows 70-71 have other_crop filled when crop_type is not "autre".}
#'   \item{Device}{ENUM02 uses two different device IDs.}
#'   \item{Enumerator productivity}{ENUM03 has ~40\% of all surveys.}
#'   \item{Comments}{Rows 60-61 contain field comments.}
#' }
#'
#' @format A data frame with 100 rows and 22 variables:
#' \describe{
#'   \item{hh_id}{Character. Household identifier (primary key).}
#'   \item{enum_id}{Character. Enumerator identifier (ENUM01-ENUM06).}
#'   \item{submission_date}{Date. Date the survey was submitted.}
#'   \item{start}{POSIXct. Interview start timestamp.}
#'   \item{end}{POSIXct. Interview end timestamp.}
#'   \item{duration}{Numeric. Interview duration in minutes.}
#'   \item{gps_lat}{Numeric. GPS latitude (decimal degrees).}
#'   \item{gps_lon}{Numeric. GPS longitude (decimal degrees).}
#'   \item{gps_accuracy}{Numeric. GPS accuracy in meters.}
#'   \item{consent}{Character. Whether respondent consented ("yes"/"no").}
#'   \item{form_version}{Character. Survey form version ("v1" or "v2").}
#'   \item{hh_size}{Integer. Number of household members.}
#'   \item{income}{Numeric. Annual household income in CFA francs.}
#'   \item{expenditure}{Numeric. Annual household expenditure in CFA francs.}
#'   \item{age_head}{Integer. Age of the household head in years.}
#'   \item{name_head}{Character. Full name of the household head.}
#'   \item{phone}{Character. Phone number of the respondent.}
#'   \item{crop_type}{Character. Primary crop type cultivated.}
#'   \item{yield_kg}{Numeric. Crop yield in kilograms per hectare.}
#'   \item{other_crop}{Character. Specify if crop_type is "autre"; NA otherwise.}
#'   \item{device_id}{Character. Identifier of the data collection device.}
#'   \item{comments}{Character. Enumerator field comments; mostly NA.}
#' }
#'
#' @source Synthetic data generated for package documentation.
#'   See \code{data-raw/survey_sample.R} for the creation script.
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
#' A small dataset of 15 back-check interviews matching records in
#' \code{\link{survey_sample}}. Contains intentional mismatches across
#' Type 1 (exact match), Type 2 (categorical), and Type 3 (continuous)
#' variables, suitable for testing the back-check reconciliation functions.
#'
#' @section Planted mismatches:
#' \describe{
#'   \item{Type 1 (consent)}{Row 3 has consent = "no" vs. "yes" in original.}
#'   \item{Type 2 (hh_size)}{Rows 5 and 8 differ by 1-2 members.}
#'   \item{Type 3 (income)}{Rows 2 and 7 differ by 35-40\%; row 10 by 5\%.}
#'   \item{Age}{Row 4 differs by 5 years.}
#'   \item{Yield}{Row 6 differs by 50\%.}
#' }
#'
#' @format A data frame with 15 rows and 9 variables:
#' \describe{
#'   \item{bc_id}{Character. Back-check interview identifier.}
#'   \item{hh_id}{Character. Household identifier matching \code{survey_sample}.}
#'   \item{bc_enum_id}{Character. Back-check enumerator identifier.}
#'   \item{consent}{Character. Consent response ("yes"/"no").}
#'   \item{hh_size}{Integer. Reported household size.}
#'   \item{income}{Numeric. Reported annual income in CFA francs.}
#'   \item{age_head}{Integer. Reported age of household head.}
#'   \item{crop_type}{Character. Reported primary crop type.}
#'   \item{yield_kg}{Numeric. Reported crop yield in kg/ha.}
#' }
#'
#' @source Synthetic data generated for package documentation.
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
#'   \item{reason}{Character. Explanation for the correction (in French).}
#' }
#'
#' @source Synthetic data generated for package documentation.
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
