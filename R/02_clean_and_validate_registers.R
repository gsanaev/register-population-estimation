# =====================================================================
# 02_clean_and_validate_registers.R
# Cleaning, Plausibility Checks, and Validation of Synthetic Registers
# ---------------------------------------------------------------------
# This script loads the synthetic administrative registers created in
# 01_generate_synthetic_registers.R and performs:
#   - structural validation of keys
#   - plausibility checks within registers
#   - consistency checks across registers
#   - rule-based cleaning
#   - creation of person-level activity indicators for later estimation
#
# Output:
#   data/clean/population_register_clean.csv
#   data/clean/employment_register_clean.csv
#   data/clean/tax_register_clean.csv
#   data/clean/education_register_clean.csv
#   data/clean/register_activity_summary.csv
# =====================================================================

# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(stringr)

# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 2. Load raw datasets
# ---------------------------------------------------------------------
population_raw <- read_csv(
  "data/raw/population_register.csv",
  show_col_types = FALSE
)

employment_raw <- read_csv(
  "data/raw/employment_register.csv",
  show_col_types = FALSE
)

tax_raw <- read_csv(
  "data/raw/tax_register.csv",
  show_col_types = FALSE
)

education_raw <- read_csv(
  "data/raw/education_register.csv",
  show_col_types = FALSE
)

# ---------------------------------------------------------------------
# 3. Basic structural validation
# ---------------------------------------------------------------------
message("Running structural validation...")

# Unique person IDs in population register
population_ids_unique <- nrow(population_raw) == n_distinct(population_raw$person_id)
if (!population_ids_unique) {
  warning("Duplicate person IDs detected in population_register.csv")
}

# Cross-register key checks
employment_missing_ids <- setdiff(employment_raw$person_id, population_raw$person_id)
tax_missing_ids        <- setdiff(tax_raw$person_id, population_raw$person_id)
education_missing_ids  <- setdiff(education_raw$person_id, population_raw$person_id)

if (length(employment_missing_ids) > 0) {
  warning("Employment register contains IDs not found in population register.")
}

if (length(tax_missing_ids) > 0) {
  warning("Tax register contains IDs not found in population register.")
}

if (length(education_missing_ids) > 0) {
  warning("Education register contains IDs not found in population register.")
}

# ---------------------------------------------------------------------
# 4. Clean population register
# ---------------------------------------------------------------------
message("Cleaning population register...")

valid_region_codes <- sprintf("R%02d", 1:12)
valid_registration_status <- c("main_residence", "secondary_residence")
valid_citizenship_groups <- c("DE", "EU", "Non-EU")

population_clean <- population_raw %>%
  clean_names() %>%
  mutate(
    registration_date = as.Date(registration_date),
    last_move_date    = as.Date(last_move_date),

    # Validation flags
    flag_duplicate_person_id = duplicated(person_id),
    flag_invalid_region      = !region_code %in% valid_region_codes | is.na(region_code),
    flag_invalid_age         = age < 0 | age > 100 | is.na(age),
    flag_missing_registration_status =
      is.na(registration_status) | !registration_status %in% valid_registration_status,
    flag_invalid_citizenship_group =
      is.na(citizenship_group) | !citizenship_group %in% valid_citizenship_groups,
    flag_date_inconsistency =
      !is.na(registration_date) & !is.na(last_move_date) & last_move_date < registration_date
  ) %>%
  mutate(
    # Rule-based cleaning
    age = if_else(age < 0 | age > 100, NA_real_, as.numeric(age)),
    registration_status = if_else(
      registration_status %in% valid_registration_status,
      registration_status,
      NA_character_
    ),
    citizenship_group = if_else(
      citizenship_group %in% valid_citizenship_groups,
      citizenship_group,
      NA_character_
    ),
    last_move_date = if_else(
      !is.na(registration_date) & !is.na(last_move_date) & last_move_date < registration_date,
      registration_date,
      last_move_date
    ),
    age_group = case_when(
      !is.na(age) & age <= 5  ~ "0-5",
      !is.na(age) & age <= 17 ~ "6-17",
      !is.na(age) & age <= 24 ~ "18-24",
      !is.na(age) & age <= 39 ~ "25-39",
      !is.na(age) & age <= 64 ~ "40-64",
      !is.na(age) & age <= 79 ~ "65-79",
      !is.na(age)             ~ "80+",
      TRUE                    ~ NA_character_
    )
  )

# ---------------------------------------------------------------------
# 5. Clean employment register
# ---------------------------------------------------------------------
message("Cleaning employment register...")

valid_employment_status <- c("employed", "marginal", "self_employed", "no_record")

employment_clean <- employment_raw %>%
  clean_names() %>%
  mutate(
    ref_date = as.Date(ref_date),

    # Validation flags
    flag_invalid_employment_status =
      !employment_status %in% valid_employment_status | is.na(employment_status),
    flag_negative_income =
      !is.na(annual_employment_income) & annual_employment_income < 0,
    flag_invalid_days_employed =
      !is.na(days_employed_last_12m) &
      (days_employed_last_12m < 0 | days_employed_last_12m > 366)
  ) %>%
  left_join(
    population_clean %>% select(person_id, age),
    by = "person_id"
  ) %>%
  mutate(
    flag_employed_under_15 =
      !is.na(age) &
      age < 15 &
      employment_status %in% c("employed", "marginal", "self_employed"),

    # Rule-based cleaning
    annual_employment_income = if_else(
      !is.na(annual_employment_income) & annual_employment_income < 0,
      NA_real_,
      annual_employment_income
    ),
    days_employed_last_12m = if_else(
      !is.na(days_employed_last_12m) &
      (days_employed_last_12m < 0 | days_employed_last_12m > 366),
      NA_real_,
      as.numeric(days_employed_last_12m)
    ),
    employment_status = if_else(
      employment_status %in% valid_employment_status,
      employment_status,
      NA_character_
    )
  )

# ---------------------------------------------------------------------
# 6. Clean tax register
# ---------------------------------------------------------------------
message("Cleaning tax register...")

tax_clean <- tax_raw %>%
  clean_names() %>%
  mutate(
    # Validation flags
    flag_invalid_tax_year = is.na(tax_year) | tax_year < 2000 | tax_year > 2030,
    flag_negative_taxable_income =
      !is.na(taxable_income) & taxable_income < 0
  ) %>%
  left_join(
    population_clean %>% select(person_id, age),
    by = "person_id"
  ) %>%
  mutate(
    flag_tax_under_14 =
      !is.na(age) &
      age < 14 &
      !is.na(tax_filing_flag) &
      tax_filing_flag == 1,

    # Rule-based cleaning
    tax_year = if_else(
      tax_year >= 2000 & tax_year <= 2030,
      tax_year,
      NA_integer_
    ),
    taxable_income = if_else(
      !is.na(taxable_income) & taxable_income < 0,
      NA_real_,
      taxable_income
    )
  )

# ---------------------------------------------------------------------
# 7. Clean education register
# ---------------------------------------------------------------------
message("Cleaning education register...")

valid_institution_types <- c("school", "university", "vocational_school")

education_clean <- education_raw %>%
  clean_names() %>%
  left_join(
    population_clean %>% select(person_id, age),
    by = "person_id"
  ) %>%
  mutate(
    # Validation flags
    flag_missing_institution_type =
      !is.na(enrolment_flag) &
      enrolment_flag == 1 &
      is.na(institution_type),

    flag_invalid_institution_type =
      !is.na(institution_type) & !institution_type %in% valid_institution_types,

    flag_implausible_enrolment_age =
      !is.na(age) &
      !is.na(enrolment_flag) &
      enrolment_flag == 1 &
      age > 35,

    # Rule-based cleaning
    institution_type = if_else(
      institution_type %in% valid_institution_types,
      institution_type,
      NA_character_
    )
  )

# ---------------------------------------------------------------------
# 8. Cross-register consistency checks and activity summary
# ---------------------------------------------------------------------
message("Building cross-register activity summary...")

employment_activity <- employment_clean %>%
  transmute(
    person_id,
    employment_signal = if_else(
      !is.na(employment_status) &
      employment_status %in% c("employed", "marginal", "self_employed"),
      1L, 0L
    ),
    flag_employed_under_15 = coalesce(flag_employed_under_15, FALSE)
  ) %>%
  group_by(person_id) %>%
  summarise(
    employment_signal = max(employment_signal, na.rm = TRUE),
    flag_employed_under_15 = max(flag_employed_under_15, na.rm = TRUE) == 1,
    .groups = "drop"
  )

tax_activity <- tax_clean %>%
  transmute(
    person_id,
    tax_signal = if_else(!is.na(tax_filing_flag) & tax_filing_flag == 1, 1L, 0L),
    flag_tax_under_14 = coalesce(flag_tax_under_14, FALSE)
  ) %>%
  group_by(person_id) %>%
  summarise(
    tax_signal = max(tax_signal, na.rm = TRUE),
    flag_tax_under_14 = max(flag_tax_under_14, na.rm = TRUE) == 1,
    .groups = "drop"
  )

education_activity <- education_clean %>%
  transmute(
    person_id,
    education_signal = if_else(!is.na(enrolment_flag) & enrolment_flag == 1, 1L, 0L),
    flag_implausible_enrolment_age = coalesce(flag_implausible_enrolment_age, FALSE)
  ) %>%
  group_by(person_id) %>%
  summarise(
    education_signal = max(education_signal, na.rm = TRUE),
    flag_implausible_enrolment_age =
      max(flag_implausible_enrolment_age, na.rm = TRUE) == 1,
    .groups = "drop"
  )

register_activity_summary <- population_clean %>%
  select(
    person_id,
    age,
    age_group,
    region_code,
    registration_status,
    resident_true,
    overcoverage_flag_true
  ) %>%
  left_join(employment_activity, by = "person_id") %>%
  left_join(tax_activity, by = "person_id") %>%
  left_join(education_activity, by = "person_id") %>%
  mutate(
    employment_signal = coalesce(employment_signal, 0L),
    tax_signal = coalesce(tax_signal, 0L),
    education_signal = coalesce(education_signal, 0L),

    n_activity_signals = employment_signal + tax_signal + education_signal,

    flag_no_activity_signal = n_activity_signals == 0,

    # Age-aware plausibility of "no activity"
    flag_potential_overcoverage = case_when(
      age_group %in% c("25-39", "40-64") & n_activity_signals == 0 ~ TRUE,
      age_group == "18-24" & n_activity_signals == 0 ~ TRUE,
      age_group == "80+"   & n_activity_signals == 0 ~ FALSE,
      age_group == "0-5"   & n_activity_signals == 0 ~ FALSE,
      TRUE ~ FALSE
    ),

    flag_employed_under_15 = coalesce(flag_employed_under_15, FALSE),
    flag_tax_under_14 = coalesce(flag_tax_under_14, FALSE),
    flag_implausible_enrolment_age =
      coalesce(flag_implausible_enrolment_age, FALSE)
  )

# ---------------------------------------------------------------------
# 9. Optional summary messages
# ---------------------------------------------------------------------
message("Summary of validation flags:")
message("Population register: invalid age cases = ",
        sum(population_clean$flag_invalid_age, na.rm = TRUE))
message("Population register: invalid region cases = ",
        sum(population_clean$flag_invalid_region, na.rm = TRUE))
message("Employment register: employed under 15 = ",
        sum(employment_clean$flag_employed_under_15, na.rm = TRUE))
message("Tax register: tax filing under 14 = ",
        sum(tax_clean$flag_tax_under_14, na.rm = TRUE))
message("Activity summary: no activity signal = ",
        sum(register_activity_summary$flag_no_activity_signal, na.rm = TRUE))
message("Activity summary: potential overcoverage = ",
        sum(register_activity_summary$flag_potential_overcoverage, na.rm = TRUE))

# ---------------------------------------------------------------------
# 10. Write cleaned datasets
# ---------------------------------------------------------------------
write_csv(
  population_clean,
  "data/clean/population_register_clean.csv"
)

write_csv(
  employment_clean,
  "data/clean/employment_register_clean.csv"
)

write_csv(
  tax_clean,
  "data/clean/tax_register_clean.csv"
)

write_csv(
  education_clean,
  "data/clean/education_register_clean.csv"
)

write_csv(
  register_activity_summary,
  "data/clean/register_activity_summary.csv"
)

message("Cleaning and validation completed successfully.")
message("Files written to data/clean/")