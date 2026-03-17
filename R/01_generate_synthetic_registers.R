# =====================================================================
# 01_generate_synthetic_registers.R
# Synthetic Register Data Generator for Population Estimation
# ---------------------------------------------------------------------
# This script creates realistic synthetic administrative registers for a
# register-based population estimation workflow.
#
# Purpose:
#   - Simulate a population register with demographic and registration
#     information
#   - Simulate linked administrative activity sources
#   - Include a hidden "true resident" status for later methodological
#     evaluation
#   - Inject realistic imperfections such as missing values, outdated
#     registrations, and contradictory signals
#
# Output:
#   data/raw/population_register.csv
#   data/raw/employment_register.csv
#   data/raw/tax_register.csv
#   data/raw/education_register.csv
#
# Notes:
#   - All data is fully synthetic
#   - No real persons or administrative records are used
#   - The project is designed to reflect methodological challenges in
#     register-based population statistics
# =====================================================================

# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(purrr)

set.seed(2026)

# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 2. Create reference structures
# ---------------------------------------------------------------------

regions <- tibble(
  region_code = sprintf("R%02d", 1:12),
  region_name = c(
    "North City", "South City", "West District", "East District",
    "Central Region", "River Region", "Mountain Region", "Lake Region",
    "Urban Belt", "Rural Belt", "Border Region", "Metro Region"
  ),
  urbanicity = c(
    "urban", "urban", "mixed", "mixed",
    "urban", "mixed", "rural", "rural",
    "urban", "rural", "mixed", "urban"
  )
)

sex_categories <- c("F", "M")
citizenship_groups <- c("DE", "EU", "Non-EU")
registration_statuses <- c("main_residence", "secondary_residence")
institution_types <- c("school", "university", "vocational_school")

# ---------------------------------------------------------------------
# 3. Generate core synthetic population register
# ---------------------------------------------------------------------

n_persons <- 50000

# Age distribution: more realistic than uniform
age_groups <- tibble(
  age_band = c("0-5", "6-17", "18-24", "25-39", "40-64", "65-79", "80+"),
  prob = c(0.06, 0.13, 0.09, 0.22, 0.30, 0.14, 0.06)
)

sample_age <- function(n) {
  sampled_bands <- sample(age_groups$age_band, size = n, replace = TRUE, prob = age_groups$prob)

  ages <- map_int(sampled_bands, function(band) {
    switch(
      band,
      "0-5"   = sample(0:5, 1),
      "6-17"  = sample(6:17, 1),
      "18-24" = sample(18:24, 1),
      "25-39" = sample(25:39, 1),
      "40-64" = sample(40:64, 1),
      "65-79" = sample(65:79, 1),
      "80+"   = sample(80:95, 1)
    )
  })

  ages
}

population_register <- tibble(
  person_id = sprintf("P%06d", 1:n_persons),
  household_id = sprintf("H%05d", sample(1:18000, n_persons, replace = TRUE)),
  region_code = sample(regions$region_code, n_persons, replace = TRUE,
                       prob = c(0.11, 0.10, 0.08, 0.07, 0.10, 0.08, 0.06, 0.05, 0.12, 0.08, 0.05, 0.10)),
  sex = sample(sex_categories, n_persons, replace = TRUE, prob = c(0.50, 0.50)),
  age = sample_age(n_persons),
  citizenship_group = sample(citizenship_groups, n_persons, replace = TRUE, prob = c(0.76, 0.14, 0.10)),
  registration_status = sample(registration_statuses, n_persons, replace = TRUE, prob = c(0.93, 0.07)),
  registration_date = sample(seq.Date(from = as.Date("2005-01-01"),
                                      to   = as.Date("2025-12-31"),
                                      by   = "day"),
                             n_persons, replace = TRUE),
  last_move_date = sample(seq.Date(from = as.Date("2010-01-01"),
                                   to   = as.Date("2025-12-31"),
                                   by   = "day"),
                          n_persons, replace = TRUE)
) %>%
  mutate(
    region_code = as.character(region_code),
    age_group = case_when(
      age <= 5  ~ "0-5",
      age <= 17 ~ "6-17",
      age <= 24 ~ "18-24",
      age <= 39 ~ "25-39",
      age <= 64 ~ "40-64",
      age <= 79 ~ "65-79",
      TRUE      ~ "80+"
    )
  )

# Ensure last_move_date is not before registration_date
population_register <- population_register %>%
  mutate(
    last_move_date = if_else(last_move_date < registration_date, registration_date, last_move_date)
  )

# ---------------------------------------------------------------------
# 4. Create hidden "true resident" status
# ---------------------------------------------------------------------
# This variable is intentionally synthetic and would not exist in real
# administrative data. It is used later to evaluate the quality of
# activity-based classification approaches.

population_register <- population_register %>%
  mutate(
    base_nonresident_prob = case_when(
      age_group == "18-24" & citizenship_group == "Non-EU" ~ 0.12,
      age_group == "25-39" & registration_status == "secondary_residence" ~ 0.10,
      age_group == "40-64" & registration_status == "secondary_residence" ~ 0.07,
      age_group == "80+" ~ 0.03,
      TRUE ~ 0.05
    ),
    resident_true = if_else(runif(n()) > base_nonresident_prob, 1L, 0L),

    # Helpful interpretation variable for later diagnostics
    overcoverage_flag_true = if_else(resident_true == 0L, 1L, 0L)
  )

# ---------------------------------------------------------------------
# 5. Generate employment register
# ---------------------------------------------------------------------
# Employment activity is more likely for working-age residents.

employment_register <- population_register %>%
  transmute(
    person_id,
    age,
    age_group,
    region_code,
    resident_true
  ) %>%
  mutate(
    eligible_for_employment = age >= 18 & age <= 67,

    employment_prob = case_when(
      resident_true == 0L ~ 0.08,
      age >= 18 & age <= 24 ~ 0.48,
      age >= 25 & age <= 39 ~ 0.78,
      age >= 40 & age <= 64 ~ 0.73,
      age >= 65 & age <= 67 ~ 0.18,
      TRUE ~ 0.02
    ),

    employment_flag = if_else(
      eligible_for_employment & runif(n()) < employment_prob,
      1L, 0L
    ),

    employment_status = case_when(
      employment_flag == 1L ~ sample(c("employed", "marginal", "self_employed"), n(), replace = TRUE,
                                     prob = c(0.78, 0.12, 0.10)),
      TRUE ~ "no_record"
    ),

    days_employed_last_12m = case_when(
      employment_status == "employed"      ~ pmin(365, pmax(20, round(rnorm(n(), mean = 290, sd = 60)))),
      employment_status == "marginal"      ~ pmin(250, pmax(5, round(rnorm(n(), mean = 110, sd = 40)))),
      employment_status == "self_employed" ~ pmin(365, pmax(30, round(rnorm(n(), mean = 250, sd = 80)))),
      TRUE ~ 0
    ),

    annual_employment_income = case_when(
      employment_status == "employed"      ~ round(rlnorm(n(), meanlog = 10.2, sdlog = 0.45), 2),
      employment_status == "marginal"      ~ round(rlnorm(n(), meanlog = 8.5,  sdlog = 0.35), 2),
      employment_status == "self_employed" ~ round(rlnorm(n(), meanlog = 10.0, sdlog = 0.70), 2),
      TRUE ~ 0
    ),

    ref_date = as.Date("2025-12-31")
  ) %>%
  select(
    person_id,
    ref_date,
    employment_status,
    days_employed_last_12m,
    annual_employment_income
  )

# Inject small missingness / administrative imperfections
employment_register <- employment_register %>%
  mutate(
    days_employed_last_12m = if_else(runif(n()) < 0.01, NA_integer_, days_employed_last_12m),
    annual_employment_income = if_else(runif(n()) < 0.01, NA_real_, annual_employment_income)
  )

# Keep only persons with a record or a small fraction of administrative "noise"
employment_register <- employment_register %>%
  filter(employment_status != "no_record" | runif(n()) < 0.02)

# ---------------------------------------------------------------------
# 6. Generate tax register
# ---------------------------------------------------------------------
# Tax activity is more likely among employed adults, self-employed, and
# some pension-age residents with taxable income.

tax_register <- population_register %>%
  transmute(
    person_id,
    age,
    resident_true
  ) %>%
  mutate(
    tax_filing_prob = case_when(
      resident_true == 0L ~ 0.06,
      age >= 18 & age <= 24 ~ 0.22,
      age >= 25 & age <= 39 ~ 0.62,
      age >= 40 & age <= 64 ~ 0.68,
      age >= 65 & age <= 79 ~ 0.28,
      age >= 80             ~ 0.12,
      TRUE ~ 0.01
    ),
    tax_filing_flag = if_else(runif(n()) < tax_filing_prob, 1L, 0L),
    tax_year = 2025L,
    taxable_income = case_when(
      tax_filing_flag == 1L & age >= 18 & age <= 24 ~ round(rlnorm(n(), meanlog = 9.2,  sdlog = 0.50), 2),
      tax_filing_flag == 1L & age >= 25 & age <= 64 ~ round(rlnorm(n(), meanlog = 10.4, sdlog = 0.55), 2),
      tax_filing_flag == 1L & age >= 65             ~ round(rlnorm(n(), meanlog = 9.5,  sdlog = 0.45), 2),
      TRUE ~ 0
    )
  ) %>%
  select(person_id, tax_year, tax_filing_flag, taxable_income) %>%
  filter(tax_filing_flag == 1L | runif(n()) < 0.015)

# Inject some imperfections
tax_register <- tax_register %>%
  mutate(
    taxable_income = if_else(runif(n()) < 0.01, NA_real_, taxable_income),
    taxable_income = if_else(runif(n()) < 0.005, -abs(taxable_income), taxable_income)
  )

# ---------------------------------------------------------------------
# 7. Generate education register
# ---------------------------------------------------------------------
# Education activity is concentrated among children, adolescents, and
# younger adults.

education_register <- population_register %>%
  transmute(
    person_id,
    age,
    resident_true
  ) %>%
  mutate(
    enrolment_prob = case_when(
      resident_true == 0L ~ 0.03,
      age >= 6  & age <= 15 ~ 0.96,
      age >= 16 & age <= 17 ~ 0.90,
      age >= 18 & age <= 24 ~ 0.42,
      age >= 25 & age <= 30 ~ 0.08,
      TRUE ~ 0.01
    ),
    enrolment_flag = if_else(runif(n()) < enrolment_prob, 1L, 0L),
    school_year = "2025/2026",
    institution_type = case_when(
      enrolment_flag == 1L & age >= 6  & age <= 17 ~ sample(c("school", "vocational_school"), n(), replace = TRUE, prob = c(0.88, 0.12)),
      enrolment_flag == 1L & age >= 18 & age <= 30 ~ sample(c("university", "vocational_school"), n(), replace = TRUE, prob = c(0.70, 0.30)),
      TRUE ~ NA_character_
    )
  ) %>%
  select(person_id, school_year, enrolment_flag, institution_type) %>%
  filter(enrolment_flag == 1L)

# Small amount of missing institution type
education_register <- education_register %>%
  mutate(
    institution_type = if_else(runif(n()) < 0.01, NA_character_, institution_type)
  )

# ---------------------------------------------------------------------
# 8. Inject imperfections into the population register
# ---------------------------------------------------------------------
# These imperfections mimic quality issues in population registers:
# missing values, delayed updates, and implausible move dates.

population_register <- population_register %>%
  mutate(
    citizenship_group = if_else(runif(n()) < 0.005, NA_character_, citizenship_group),
    registration_status = if_else(runif(n()) < 0.003, NA_character_, registration_status),
    last_move_date = if_else(runif(n()) < 0.004,
                             last_move_date + sample(1:120, n(), replace = TRUE),
                             last_move_date),
    # A few implausibly high ages for later cleaning checks
    age = if_else(runif(n()) < 0.002, sample(97:110, n(), replace = TRUE), age)
  )

# Recompute age_group after age perturbation
population_register <- population_register %>%
  mutate(
    age_group = case_when(
      age <= 5  ~ "0-5",
      age <= 17 ~ "6-17",
      age <= 24 ~ "18-24",
      age <= 39 ~ "25-39",
      age <= 64 ~ "40-64",
      age <= 79 ~ "65-79",
      TRUE      ~ "80+"
    )
  )

# ---------------------------------------------------------------------
# 9. Write datasets to disk
# ---------------------------------------------------------------------

write_csv(population_register, "data/raw/population_register.csv")
write_csv(employment_register, "data/raw/employment_register.csv")
write_csv(tax_register,        "data/raw/tax_register.csv")
write_csv(education_register,  "data/raw/education_register.csv")

message("Synthetic administrative registers generated successfully.")
message("Files written to data/raw/")