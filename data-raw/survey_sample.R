## code to prepare `survey_sample`, `backcheck_sample`, and `corrections_sample`

set.seed(42)

# --- Helper data ---
west_african_first <- c(
 "Abdoulaye", "Aminata", "Boureima", "Fatimata", "Hamidou",
 "Issa", "Kadiatou", "Moussa", "Ousmane", "Rasmata",
 "Salamata", "Souleymane", "Zenabou", "Ibrahim", "Mariam",
 "Adama", "Bintou", "Dramane", "Fanta", "Karim",
 "Lassina", "Noufou", "Poko", "Seydou", "Tene",
 "Youssouf", "Alimata", "Boubacar", "Djamilatou", "Hamado"
)
west_african_last <- c(
 "Ouedraogo", "Sawadogo", "Kabore", "Traore", "Coulibaly",
 "Diallo", "Compaore", "Zongo", "Bamba", "Sanou",
 "Konate", "Ilboudo", "Nikiema", "Zoungrana", "Bationo",
 "Sanogo", "Sorgho", "Thiombiano", "Dembele", "Yameogo"
)

crop_types <- c("mil", "sorgho", "mais", "riz", "niebe",
                "arachide", "sesame", "coton", "autre")

enumerator_ids <- c("ENUM01", "ENUM02", "ENUM03", "ENUM04", "ENUM05", "ENUM06")
device_ids <- c("DEV_A1", "DEV_A2", "DEV_B1", "DEV_C1", "DEV_D1", "DEV_E1")

n <- 100

# --- Build survey_sample ---

# hh_id: unique with 3 intentional duplicates (rows 98-100 duplicate 1-3)
hh_ids <- sprintf("HH_%04d", 1:n)
hh_ids[98] <- hh_ids[1]
hh_ids[99] <- hh_ids[2]
hh_ids[100] <- hh_ids[3]

# enum_id: ENUM03 is suspiciously productive (40 surveys)
enum_probs <- c(0.12, 0.12, 0.40, 0.12, 0.12, 0.12)
enum_ids <- sample(enumerator_ids, n, replace = TRUE, prob = enum_probs)

# Dates: mostly within the collection window 2024-01-01 to 2024-02-28
base_dates <- as.Date("2024-01-01") + sample(0:58, n, replace = TRUE)
# Row 95: future date
base_dates[95] <- Sys.Date() + 30
# Row 96: outside collection window (too early)
base_dates[96] <- as.Date("2023-11-01")

# Start/end times
start_hours <- sample(7:16, n, replace = TRUE)
start_minutes <- sample(0:59, n, replace = TRUE)
start_times <- as.POSIXct(
  paste(base_dates, sprintf("%02d:%02d:00", start_hours, start_minutes)),
  tz = "UTC"
)

# Duration: mostly 25-90 min; row 90 very short, row 91 very long
durations <- round(runif(n, 25, 90), 1)
durations[90] <- 3   # too short
durations[91] <- 210 # too long

end_times <- start_times + durations * 60

# GPS: around Ouagadougou (~12.37, -1.52)
gps_lat <- round(rnorm(n, mean = 12.37, sd = 0.03), 6)
gps_lon <- round(rnorm(n, mean = -1.52, sd = 0.03), 6)
gps_accuracy <- round(runif(n, 3, 10), 1)

# Row 85: null island
gps_lat[85] <- 0
gps_lon[85] <- 0
gps_accuracy[85] <- 5.0

# Row 86: swapped lat/lon
gps_lat[86] <- -1.52
gps_lon[86] <- 12.37

# Row 87: poor accuracy
gps_accuracy[87] <- 150

# Consent: mostly "yes", 2 "no"
consent <- rep("yes", n)
consent[c(92, 93)] <- "no"

# Form version
form_version <- rep("v1", n)
form_version[c(88, 89)] <- "v2"

# hh_size: 1-15 with one implausible
hh_size <- sample(2:12, n, replace = TRUE)
hh_size[80] <- 50

# Income (CFA francs): realistic range 50k-500k
income <- round(runif(n, 50000, 500000), 0)
income[75] <- -15000       # negative
income[76] <- 5000000      # extreme outlier
income[77] <- 8500000      # extreme outlier

# Expenditure (CFA francs)
expenditure <- round(income * runif(n, 0.4, 0.9), 0)
# Row 78: expenditure >> income
expenditure[78] <- income[78] * 4

# Age of household head
age_head <- sample(25:65, n, replace = TRUE)
age_head[82] <- 3  # implausible

# Name of household head
name_head <- paste(
  sample(west_african_first, n, replace = TRUE),
  sample(west_african_last, n, replace = TRUE)
)
name_head[83] <- "test"   # test entry
name_head[84] <- "A"      # single character

# Phone numbers (Burkina format: 7X XX XX XX)
phone <- sprintf("7%d %02d %02d %02d",
                 sample(0:9, n, replace = TRUE),
                 sample(10:99, n, replace = TRUE),
                 sample(10:99, n, replace = TRUE),
                 sample(10:99, n, replace = TRUE))
phone[81] <- "123"  # too short

# Crop type
crop_type <- sample(crop_types, n, replace = TRUE)

# Yield (kg/ha)
yield_kg <- round(runif(n, 200, 2500), 0)
yield_kg[74] <- 15000  # extreme outlier
yield_kg[73] <- 18000  # extreme outlier

# Other crop: mostly NA, a few with text when crop_type == "autre"
other_crop <- rep(NA_character_, n)
autre_rows <- which(crop_type == "autre")
if (length(autre_rows) > 0) {
  other_crop[autre_rows] <- sample(
    c("fonio", "patate douce", "igname", "manioc"),
    length(autre_rows), replace = TRUE
  )
}
# Add a couple non-"autre" rows with other_crop filled (for check testing)
other_crop[70] <- "oignon"
other_crop[71] <- "tomate"

# Device IDs: map from enum, but ENUM02 uses two devices
device_map <- c(
  ENUM01 = "DEV_A1", ENUM02 = "DEV_B1", ENUM03 = "DEV_C1",
  ENUM04 = "DEV_D1", ENUM05 = "DEV_E1", ENUM06 = "DEV_A2"
)
device_id <- unname(device_map[enum_ids])
# Make ENUM02 use a second device for some entries
enum02_rows <- which(enum_ids == "ENUM02")
if (length(enum02_rows) >= 2) {
  device_id[enum02_rows[1:min(2, length(enum02_rows))]] <- "DEV_B2"
}

# Comments
comments <- rep(NA_character_, n)
comments[60] <- "Le chef de menage etait absent, entretien avec l'epouse"
comments[61] <- "Route inondee, difficulte d'acces au village"

# Submission date (matches config variable name "submission_date")
submission_date <- base_dates

# Build the data frame
survey_sample <- data.frame(
  hh_id           = hh_ids,
  enum_id         = enum_ids,
  submission_date = submission_date,
  start           = start_times,
  end             = end_times,
  duration        = durations,
  gps_lat         = gps_lat,
  gps_lon         = gps_lon,
  gps_accuracy    = gps_accuracy,
  consent         = consent,
  form_version    = form_version,
  hh_size         = as.integer(hh_size),
  income          = income,
  expenditure     = expenditure,
  age_head        = as.integer(age_head),
  name_head       = name_head,
  phone           = phone,
  crop_type       = crop_type,
  yield_kg        = yield_kg,
  other_crop      = other_crop,
  device_id       = device_id,
  comments        = comments,
  stringsAsFactors = FALSE
)

# --- Build backcheck_sample ---

# Select 15 hh_ids from survey (first occurrence only, no duplicates)
bc_hh_ids <- hh_ids[c(5, 12, 18, 25, 33, 40, 47, 55, 60, 65,
                       70, 74, 78, 82, 88)]

set.seed(123)
bc_enum_ids <- sample(c("BC_ENUM01", "BC_ENUM02", "BC_ENUM03"), 15,
                      replace = TRUE)

# Pull original values and introduce mismatches
orig_rows <- match(bc_hh_ids, hh_ids)
bc_hh_size    <- survey_sample$hh_size[orig_rows]
bc_income     <- survey_sample$income[orig_rows]
bc_crop_type  <- survey_sample$crop_type[orig_rows]
bc_consent    <- survey_sample$consent[orig_rows]
bc_age_head   <- survey_sample$age_head[orig_rows]
bc_yield_kg   <- survey_sample$yield_kg[orig_rows]

# Type 1 mismatches (consent — should match exactly)
bc_consent[3] <- "no"  # was "yes" in original

# Type 2 mismatches (hh_size — categorical/ordinal, small difference ok)
bc_hh_size[5] <- bc_hh_size[5] + 2
bc_hh_size[8] <- bc_hh_size[8] - 1

# Type 3 mismatches (income — continuous, relative difference)
bc_income[2]  <- round(bc_income[2] * 1.35, 0)   # 35% difference
bc_income[7]  <- round(bc_income[7] * 0.60, 0)   # 40% difference
bc_income[10] <- round(bc_income[10] * 1.05, 0)  # 5% difference (within tolerance)

# Age mismatch
bc_age_head[4] <- bc_age_head[4] + 5

# Yield mismatch
bc_yield_kg[6] <- round(bc_yield_kg[6] * 1.50, 0)

backcheck_sample <- data.frame(
  bc_id       = sprintf("BC_%03d", 1:15),
  hh_id       = bc_hh_ids,
  bc_enum_id  = bc_enum_ids,
  consent     = bc_consent,
  hh_size     = as.integer(bc_hh_size),
  income      = bc_income,
  age_head    = as.integer(bc_age_head),
  crop_type   = bc_crop_type,
  yield_kg    = bc_yield_kg,
  stringsAsFactors = FALSE
)

# --- Build corrections_sample ---

corrections_sample <- data.frame(
  hh_id     = c("HH_0080", "HH_0075", "HH_0082", "HH_0083", "HH_0081"),
  variable  = c("hh_size", "income", "age_head", "name_head", "phone"),
  old_value = c("50", "-15000", "3", "test", "123"),
  new_value = c("5", "150000", "43", "Adama Sawadogo", "70 12 34 56"),
  reason    = c(
    "Erreur de saisie: 50 au lieu de 5",
    "Valeur negative corrigee apres verification terrain",
    "Age invraisemblable: enfant de 3 ans comme chef de menage",
    "Entree de test a remplacer par le vrai nom",
    "Numero de telephone incomplet corrige"
  ),
  stringsAsFactors = FALSE
)

# --- Save datasets ---
usethis::use_data(survey_sample, backcheck_sample, corrections_sample,
                  overwrite = TRUE)
