## code to prepare `survey_sample`, `backcheck_sample`, and `corrections_sample`
## Based on REAL survey data from the World Bank iehfc package
## (Living with HIV Follow-Up 2 survey, Rwanda, 2023)

# --- Load real data from iehfc package ---
if (!requireNamespace("iehfc", quietly = TRUE)) {
  stop("Install the iehfc package first: install.packages('iehfc')")
}

raw <- read.csv(
  system.file("test_data/LWH_FUP2_raw_data.csv", package = "iehfc"),
  stringsAsFactors = FALSE
)

n <- nrow(raw)  # 1206 observations
set.seed(42)

# --- Core columns from real data ---
hh_id           <- as.character(raw$hhid)
enum_id         <- sprintf("ENUM%03d", raw$enumerator)
submission_date <- as.Date(raw$submissiondate, format = "%m/%d/%Y")
device_id       <- as.character(raw$device_id)
form_version    <- ifelse(raw$formdef_version == 2305021814, "v2", "v1")
province        <- raw$province
district        <- raw$district
sector          <- raw$sector
village         <- raw$village
income          <- raw$inc_1
income_crop     <- raw$inc_2
expenditure     <- raw$exp_25_1
crop_type       <- raw$a_crop_c1_p1
crop_other      <- raw$a_crop_c1_p1_other
yield_kg        <- raw$crp08qa_c1_p1
crop_sold_qty   <- raw$crp09qa_c1_p1
crop_revenue    <- raw$crp10a_c1_p1

# --- Synthetic but realistic augmented columns ---

# GPS: centered on Kayonza (-1.85, 30.55) and Rwamagana (-1.95, 30.43)
gps_lat <- ifelse(
  district == "Kayonza",
  round(rnorm(n, mean = -1.85, sd = 0.08), 6),
  round(rnorm(n, mean = -1.95, sd = 0.08), 6)
)
gps_lon <- ifelse(
  district == "Kayonza",
  round(rnorm(n, mean = 30.55, sd = 0.10), 6),
  round(rnorm(n, mean = 30.43, sd = 0.10), 6)
)
gps_accuracy <- round(runif(n, 3, 15), 1)

# Start/end times: realistic interview times
start_hours   <- sample(7:16, n, replace = TRUE)
start_minutes <- sample(0:59, n, replace = TRUE)
start <- as.POSIXct(
  paste(submission_date, sprintf("%02d:%02d:00", start_hours, start_minutes)),
  tz = "Africa/Kigali"
)

# Duration: 25-90 min with realistic distribution
duration <- round(rlnorm(n, meanlog = log(40), sdlog = 0.35), 1)
end <- start + duration * 60

# Consent
consent <- rep("yes", n)

# Household size: Poisson + 1, realistic for Rwanda
hh_size <- as.integer(rpois(n, lambda = 4) + 1)

# Rwandan names
rwandan_first <- c(
  "Jean", "Marie", "Pierre", "Therese", "Emmanuel", "Claudine",
  "Francois", "Jeanne", "Joseph", "Vestine", "Patrick", "Alphonsine",
  "Eric", "Goretti", "Innocent", "Dancille", "Alexis", "Esperance",
  "Denis", "Jacqueline", "Augustin", "Beata", "Celestin", "Consolee",
  "Dieudonne", "Felicite", "Gabriel", "Illuminee", "Janvier", "Bernadette"
)
rwandan_last <- c(
  "Uwimana", "Habimana", "Mukamana", "Niyonzima", "Ndayisaba",
  "Uwera", "Bizimana", "Mukagatare", "Nsengiyumva", "Nyiraneza",
  "Hakizimana", "Mukamusoni", "Ingabire", "Twagirimana", "Mutesi",
  "Nshimiyimana", "Umutoni", "Kamanzi", "Mukeshimana", "Rugamba"
)
name_head <- paste(
  sample(rwandan_first, n, replace = TRUE),
  sample(rwandan_last, n, replace = TRUE)
)

# Phone numbers: Rwanda format 078X XXX XXX / 072X / 073X
phone_prefix <- sample(c("078", "072", "073"), n, replace = TRUE)
phone <- sprintf("%s %03d %03d",
                 phone_prefix,
                 sample(100:999, n, replace = TRUE),
                 sample(100:999, n, replace = TRUE))

# Comments: mostly NA
comments <- rep(NA_character_, n)

# --- Plant additional quality issues ---
# (The real data already has: 3 duplicate hhids, -88/-66 sentinel values in income,
#  extreme outliers, 2 form versions)

# GPS null island (row 1100)
gps_lat[1100] <- 0
gps_lon[1100] <- 0
gps_accuracy[1100] <- 5.0

# GPS swapped lat/lon (row 1101)
gps_lat[1101] <- 30.55   # should be longitude
gps_lon[1101] <- -1.85   # should be latitude

# GPS poor accuracy (row 1102)
gps_accuracy[1102] <- 200

# Very short duration (row 1103)
duration[1103] <- 2
end[1103] <- start[1103] + 120

# Very long duration (row 1104)
duration[1104] <- 300
end[1104] <- start[1104] + 18000

# Future date (row 1105)
submission_date[1105] <- Sys.Date() + 30
start[1105] <- as.POSIXct(paste(submission_date[1105], "09:00:00"), tz = "Africa/Kigali")
end[1105] <- start[1105] + 2400

# No consent (rows 1106-1107)
consent[1106] <- "no"
consent[1107] <- "no"

# Implausible household size (row 1108)
hh_size[1108] <- 55L

# Test name (row 1109)
name_head[1109] <- "test"

# Single character name (row 1110)
name_head[1110] <- "X"

# Invalid phone (row 1111)
phone[1111] <- "12"

# Comments for paradata check
comments[50] <- "Le chef de menage etait absent, interview avec l'epouse"
comments[51] <- "Route inondee, difficulte d'acces"
comments[200] <- "Respondent was hesitant, needed extra explanation"

# --- Build survey_sample ---
survey_sample <- data.frame(
  hh_id           = hh_id,
  enum_id         = enum_id,
  submission_date = submission_date,
  start           = start,
  end             = end,
  duration        = duration,
  gps_lat         = gps_lat,
  gps_lon         = gps_lon,
  gps_accuracy    = gps_accuracy,
  consent         = consent,
  form_version    = form_version,
  hh_size         = hh_size,
  income          = income,
  income_crop     = income_crop,
  expenditure     = expenditure,
  crop_type       = crop_type,
  crop_other      = crop_other,
  yield_kg        = yield_kg,
  crop_sold_qty   = crop_sold_qty,
  crop_revenue    = crop_revenue,
  province        = province,
  district        = district,
  sector          = sector,
  village         = village,
  name_head       = name_head,
  phone           = phone,
  device_id       = device_id,
  comments        = comments,
  stringsAsFactors = FALSE
)

# --- Build backcheck_sample ---
# Select 20 real unique hh_ids (non-duplicated)
unique_ids <- unique(hh_id)
bc_hh_ids <- unique_ids[c(10, 25, 50, 75, 100, 150, 200, 250, 300,
                           350, 400, 450, 500, 550, 600, 650, 700,
                           750, 800, 850)]
bc_rows <- match(bc_hh_ids, hh_id)

set.seed(123)
bc_enum_ids <- sample(c("BC_ENUM01", "BC_ENUM02", "BC_ENUM03"), 20,
                      replace = TRUE)

# Pull original values
bc_consent   <- survey_sample$consent[bc_rows]
bc_hh_size   <- survey_sample$hh_size[bc_rows]
bc_income    <- survey_sample$income[bc_rows]
bc_crop_type <- survey_sample$crop_type[bc_rows]
bc_yield_kg  <- survey_sample$yield_kg[bc_rows]

# Introduce mismatches
# Type 1: consent should match exactly
bc_consent[3] <- "no"  # mismatch

# Type 2: hh_size small differences
bc_hh_size[5] <- bc_hh_size[5] + 2L
bc_hh_size[8] <- max(1L, bc_hh_size[8] - 1L)
bc_hh_size[12] <- bc_hh_size[12] + 1L

# Type 3: income continuous tolerance
bc_income[2]  <- round(bc_income[2] * 1.40, 0)   # 40% difference
bc_income[7]  <- round(bc_income[7] * 0.55, 0)   # 45% difference
bc_income[10] <- round(bc_income[10] * 1.05, 0)  # 5% (within tolerance)
bc_income[15] <- round(bc_income[15] * 1.25, 0)  # 25% difference

# Yield mismatch
bc_yield_kg[6]  <- round(bc_yield_kg[6] * 1.60, 0)  # 60% difference
bc_yield_kg[14] <- round(bc_yield_kg[14] * 0.70, 0)  # 30% difference

backcheck_sample <- data.frame(
  bc_id       = sprintf("BC_%03d", seq_along(bc_hh_ids)),
  hh_id       = bc_hh_ids,
  bc_enum_id  = bc_enum_ids,
  consent     = bc_consent,
  hh_size     = as.integer(bc_hh_size),
  income      = bc_income,
  crop_type   = bc_crop_type,
  yield_kg    = bc_yield_kg,
  stringsAsFactors = FALSE
)

# --- Build corrections_sample ---
corrections_sample <- data.frame(
  hh_id     = hh_id[c(1108, 1109, 1110, 1111, 1103)],
  variable  = c("hh_size", "name_head", "name_head", "phone", "duration"),
  old_value = c("55", "test", "X", "12", "2"),
  new_value = c("5", "Emmanuel Habimana", "Xavier Niyonzima", "078 123 456", "40"),
  reason    = c(
    "Erreur de saisie: 55 au lieu de 5 membres",
    "Entree de test a remplacer par le vrai nom",
    "Nom incomplet corrige apres verification",
    "Numero de telephone incomplet",
    "Duree trop courte - enqueteur a confirme 40 minutes"
  ),
  stringsAsFactors = FALSE
)

# --- Save datasets ---
usethis::use_data(survey_sample, backcheck_sample, corrections_sample,
                  overwrite = TRUE)

cat("Datasets saved:\n")
cat("  survey_sample:", nrow(survey_sample), "x", ncol(survey_sample), "\n")
cat("  backcheck_sample:", nrow(backcheck_sample), "x", ncol(backcheck_sample), "\n")
cat("  corrections_sample:", nrow(corrections_sample), "x", ncol(corrections_sample), "\n")
