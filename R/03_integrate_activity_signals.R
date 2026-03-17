# =====================================================================
# 03_integrate_activity_signals.R
# Integration of Cleaned Registers and Construction of Activity Signals
# ---------------------------------------------------------------------
# This script loads the cleaned synthetic registers and builds an
# analysis-ready person-level dataset for register-based population
# estimation.
#
# It performs:
#   - integration of cleaned administrative sources
#   - derivation of activity-based indicators ("Lebenszeichen")
#   - rule-based plausibility classification
#   - creation of regional summary outputs
#
# Output:
#   data/processed/person_register_integrated.csv
#   data/processed/region_activity_summary.csv
# =====================================================================

# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(janitor)

# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 2. Load cleaned datasets
# ---------------------------------------------------------------------
population_clean <- read_csv(
  "data/clean/population_register_clean.csv",
  show_col_types = FALSE
)

employment_clean <- read_csv(
  "data/clean/employment_register_clean.csv",
  show_col_types = FALSE
)

tax_clean <- read_csv(
  "data/clean/tax_register_clean.csv",
  show_col_types = FALSE
)

education_clean <- read_csv(
  "data/clean/education_register_clean.csv",
  show_col_types = FALSE
)

activity_summary <- read_csv(
  "data/clean/register_activity_summary.csv",
  show_col_types = FALSE
)

# ---------------------------------------------------------------------
# 3. Prepare compact source summaries for joining
# ---------------------------------------------------------------------
message("Preparing source-specific summaries...")

# Helper functions -----------------------------------------------------
safe_max_numeric <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

safe_max_flag <- function(x) {
  if (all(is.na(x))) 0L else max(x, na.rm = TRUE)
}

safe_first_char <- function(x) {
  x_non_missing <- x[!is.na(x)]
  if (length(x_non_missing) == 0) NA_character_ else x_non_missing[1]
}

# ---------------------------------------------------------------------
# Employment summary
# ---------------------------------------------------------------------
employment_summary <- employment_clean %>%
  group_by(person_id) %>%
  summarise(
    employment_status_main = safe_first_char(employment_status),
    days_employed_last_12m = safe_max_numeric(days_employed_last_12m),
    annual_employment_income = safe_max_numeric(annual_employment_income),
    .groups = "drop"
  )

# ---------------------------------------------------------------------
# Tax summary
# ---------------------------------------------------------------------
tax_summary <- tax_clean %>%
  group_by(person_id) %>%
  summarise(
    tax_filing_flag = safe_max_flag(tax_filing_flag),
    taxable_income = safe_max_numeric(taxable_income),
    .groups = "drop"
  )

# ---------------------------------------------------------------------
# Education summary
# ---------------------------------------------------------------------
education_summary <- education_clean %>%
  group_by(person_id) %>%
  summarise(
    enrolment_flag = safe_max_flag(enrolment_flag),
    institution_type_main = safe_first_char(institution_type),
    .groups = "drop"
  )

# ---------------------------------------------------------------------
# 4. Build integrated person-level analytical dataset
# ---------------------------------------------------------------------
message("Integrating cleaned registers...")

person_register_integrated <- activity_summary %>%
  left_join(
    population_clean %>%
      select(
        person_id,
        household_id,
        sex,
        age,
        age_group,
        region_code,
        citizenship_group,
        registration_status,
        registration_date,
        last_move_date,
        resident_true,
        overcoverage_flag_true
      ),
    by = c(
      "person_id",
      "age",
      "age_group",
      "region_code",
      "registration_status",
      "resident_true",
      "overcoverage_flag_true"
    )
  ) %>%
  left_join(employment_summary, by = "person_id") %>%
  left_join(tax_summary, by = "person_id") %>%
  left_join(education_summary, by = "person_id") %>%
  mutate(
    employment_signal = coalesce(employment_signal, 0),
    tax_signal = coalesce(tax_signal, 0),
    education_signal = coalesce(education_signal, 0),

    days_employed_last_12m = as.numeric(days_employed_last_12m),
    annual_employment_income = as.numeric(annual_employment_income),
    taxable_income = as.numeric(taxable_income),

    signal_count = employment_signal + tax_signal + education_signal,

    signal_strength = case_when(
      signal_count >= 3 ~ "high",
      signal_count == 2 ~ "medium",
      signal_count == 1 ~ "low",
      TRUE ~ "none"
    ),

    # Rule-based residence plausibility
    likely_resident_rule = case_when(
      signal_count >= 1 ~ 1L,
      age_group %in% c("0-5", "6-17") & education_signal == 1 ~ 1L,
      age_group == "80+" & signal_count == 0 ~ 1L,
      TRUE ~ 0L
    ),

    # More refined risk categorisation
    overcoverage_risk = case_when(
      age_group %in% c("25-39", "40-64") & signal_count == 0 ~ "high",
      age_group == "18-24" & signal_count == 0 ~ "high",
      age_group %in% c("65-79") & signal_count == 0 ~ "medium",
      age_group %in% c("0-5", "6-17") & signal_count == 0 ~ "low",
      age_group == "80+" & signal_count == 0 ~ "low",
      signal_count == 1 ~ "medium",
      signal_count >= 2 ~ "low",
      TRUE ~ "medium"
    ),

    # Additional interpretable indicators
    economic_activity_flag = if_else(
      employment_signal == 1 | tax_signal == 1,
      1L, 0L
    ),

    social_activity_flag = if_else(
      education_signal == 1,
      1L, 0L
    )
  ) %>%
  arrange(person_id)

# ---------------------------------------------------------------------
# 5. Consistency checks
# ---------------------------------------------------------------------
message("Running consistency checks...")

# Check for duplicate person IDs after integration
if (nrow(person_register_integrated) != n_distinct(person_register_integrated$person_id)) {
  warning("Duplicate person IDs detected in integrated person file.")
}

# Check that signal_count is within plausible bounds
invalid_signal_count <- person_register_integrated %>%
  filter(signal_count < 0 | signal_count > 3)

if (nrow(invalid_signal_count) > 0) {
  warning("Invalid signal_count values detected.")
}

# Check distribution of rule-based likely residence
message("Distribution of likely_resident_rule:")
print(table(person_register_integrated$likely_resident_rule, useNA = "ifany"))

message("Distribution of overcoverage_risk:")
print(table(person_register_integrated$overcoverage_risk, useNA = "ifany"))

# ---------------------------------------------------------------------
# 6. Create regional summary output
# ---------------------------------------------------------------------
message("Building regional summary...")

region_activity_summary <- person_register_integrated %>%
  group_by(region_code) %>%
  summarise(
    population_register_count = n(),
    n_likely_residents = sum(likely_resident_rule == 1, na.rm = TRUE),
    n_high_risk = sum(overcoverage_risk == "high", na.rm = TRUE),
    n_no_activity = sum(signal_count == 0, na.rm = TRUE),
    mean_signal_count = mean(signal_count, na.rm = TRUE),
    employment_signal_rate = mean(employment_signal, na.rm = TRUE),
    tax_signal_rate = mean(tax_signal, na.rm = TRUE),
    education_signal_rate = mean(education_signal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region_code)

# ---------------------------------------------------------------------
# 7. Write processed datasets
# ---------------------------------------------------------------------
write_csv(
  person_register_integrated,
  "data/processed/person_register_integrated.csv"
)

write_csv(
  region_activity_summary,
  "data/processed/region_activity_summary.csv"
)

message("Integration and activity-signal construction completed successfully.")
message("Files written to data/processed/")