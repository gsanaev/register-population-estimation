# =====================================================================
# 01_generate_synthetic_registers.R
# Synthetic Register Data Generator for Population Estimation
# Version 2
# ---------------------------------------------------------------------
# This script creates a synthetic population universe and several
# imperfect administrative registers for a register-based population
# estimation workflow.
#
# Core design:
#   1. Generate a hidden synthetic "true" population.
#   2. Generate an address structure.
#   3. Derive an imperfect population register from the true population.
#   4. Introduce both:
#        - overcoverage (registered persons who are no longer residents)
#        - undercoverage (true residents missing from the population register)
#   5. Generate employment, tax, and education registers independently
#      from the synthetic truth.
#
# This design allows administrative activity sources to contain persons
# who are absent from the population register and therefore creates a
# genuine synthetic undercoverage problem.
#
# Outputs:
#   data/raw/synthetic_population_truth.csv
#   data/raw/address_register.csv
#   data/raw/population_register.csv
#   data/raw/employment_register.csv
#   data/raw/tax_register.csv
#   data/raw/education_register.csv
#
# Notes:
#   - All data are fully synthetic.
#   - No real persons or administrative records are used.
#   - Simulation probabilities are illustrative methodological choices,
#     not empirical estimates for Germany or any specific statistical
#     authority.
#   - The project does not reproduce an official Destatis production
#     system or methodology.
# =====================================================================


# ---------------------------------------------------------------------
# 0. Load packages and define reproducibility settings
# ---------------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)

set.seed(2026)

reference_date <- as.Date("2025-12-31")


# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------

dir.create(
  "data/raw",
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------------------
# 2. Create regional and demographic reference structures
# ---------------------------------------------------------------------

regions <- tibble(
  region_code = sprintf("R%02d", 1:12),

  region_name = c(
    "North City",
    "South City",
    "West District",
    "East District",
    "Central Region",
    "River Region",
    "Mountain Region",
    "Lake Region",
    "Urban Belt",
    "Rural Belt",
    "Border Region",
    "Metro Region"
  ),

  urbanicity = c(
    "urban",
    "urban",
    "mixed",
    "mixed",
    "urban",
    "mixed",
    "rural",
    "rural",
    "urban",
    "rural",
    "mixed",
    "urban"
  ),

  population_weight = c(
    0.11, 0.10, 0.08, 0.07,
    0.10, 0.08, 0.06, 0.05,
    0.12, 0.08, 0.05, 0.10
  )
)

sex_categories <- c("F", "M")

citizenship_groups <- c(
  "DE",
  "EU",
  "Non-EU"
)

registration_statuses <- c(
  "main_residence",
  "secondary_residence"
)


# ---------------------------------------------------------------------
# 3. Define age distribution and helper function
# ---------------------------------------------------------------------

age_groups <- tibble(
  age_band = c(
    "0-5",
    "6-17",
    "18-24",
    "25-39",
    "40-64",
    "65-79",
    "80+"
  ),

  prob = c(
    0.06,
    0.13,
    0.09,
    0.22,
    0.30,
    0.14,
    0.06
  )
)

sample_age <- function(n, probabilities = age_groups$prob) {

  sampled_bands <- sample(
    age_groups$age_band,
    size = n,
    replace = TRUE,
    prob = probabilities
  )

  map_int(
    sampled_bands,
    function(band) {

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
    }
  )
}


# ---------------------------------------------------------------------
# 4. Generate synthetic address register
# ---------------------------------------------------------------------

n_addresses <- 18000

address_register <- tibble(
  address_id = sprintf(
    "A%06d",
    1:n_addresses
  ),

  region_code = sample(
    regions$region_code,
    size = n_addresses,
    replace = TRUE,
    prob = regions$population_weight
  )
) %>%

  left_join(
    regions %>%
      select(
        region_code,
        region_name,
        urbanicity
      ),
    by = "region_code"
  ) %>%

  mutate(
    municipality_number = sample(
      1:4,
      size = n(),
      replace = TRUE
    ),

    municipality_code = paste0(
      region_code,
      "-M",
      sprintf(
        "%02d",
        municipality_number
      )
    ),

    address_type = case_when(

      urbanicity == "urban" ~ sample(
        c(
          "single_family",
          "multi_family",
          "large_residential"
        ),
        size = n(),
        replace = TRUE,
        prob = c(
          0.25,
          0.55,
          0.20
        )
      ),

      urbanicity == "mixed" ~ sample(
        c(
          "single_family",
          "multi_family",
          "large_residential"
        ),
        size = n(),
        replace = TRUE,
        prob = c(
          0.45,
          0.45,
          0.10
        )
      ),

      TRUE ~ sample(
        c(
          "single_family",
          "multi_family",
          "large_residential"
        ),
        size = n(),
        replace = TRUE,
        prob = c(
          0.70,
          0.27,
          0.03
        )
      )
    )
  ) %>%

  select(
    address_id,
    region_code,
    region_name,
    municipality_code,
    urbanicity,
    address_type
  )


# Addresses with larger residential structures receive a higher
# probability of hosting multiple synthetic households.

address_sampling_weights <- ifelse(
  address_register$address_type == "single_family",
  1,
  ifelse(
    address_register$address_type == "multi_family",
    2.5,
    6
  )
)


# ---------------------------------------------------------------------
# 5. Generate households for the true resident population
# ---------------------------------------------------------------------

n_true_residents <- 50000
n_former_residents <- 3000

# Generate enough candidate household sizes and truncate the final
# household so that the total number of residents is exactly 50,000.

household_size_pool <- sample(
  1:6,
  size = 40000,
  replace = TRUE,
  prob = c(
    0.40,
    0.32,
    0.13,
    0.09,
    0.04,
    0.02
  )
)

cumulative_household_size <- cumsum(
  household_size_pool
)

last_household_index <- which(
  cumulative_household_size >= n_true_residents
)[1]

household_sizes <- household_size_pool[
  1:last_household_index
]

household_sizes[
  last_household_index
] <- n_true_residents -
  sum(
    household_sizes[
      -last_household_index
    ]
  )

n_households <- length(
  household_sizes
)

household_register <- tibble(
  household_id = sprintf(
    "H%06d",
    1:n_households
  ),

  household_size = household_sizes,

  address_id = sample(
    address_register$address_id,
    size = n_households,
    replace = TRUE,
    prob = address_sampling_weights
  )
)


# ---------------------------------------------------------------------
# 6. Generate the hidden synthetic true resident population
# ---------------------------------------------------------------------

resident_household_assignment <- tibble(
  person_id = sprintf(
    "P%06d",
    1:n_true_residents
  ),

  true_household_id = rep(
    household_register$household_id,
    household_register$household_size
  )
)

true_residents <- resident_household_assignment %>%

  left_join(
    household_register %>%
      select(
        household_id,
        address_id
      ),
    by = c(
      "true_household_id" = "household_id"
    )
  ) %>%

  left_join(
    address_register %>%
      select(
        address_id,
        region_code,
        municipality_code
      ),
    by = "address_id"
  ) %>%

  transmute(
    person_id,

    true_resident = 1L,

    sex = sample(
      sex_categories,
      size = n(),
      replace = TRUE,
      prob = c(
        0.50,
        0.50
      )
    ),

    age = sample_age(
      n()
    ),

    age_group = case_when(
      age <= 5  ~ "0-5",
      age <= 17 ~ "6-17",
      age <= 24 ~ "18-24",
      age <= 39 ~ "25-39",
      age <= 64 ~ "40-64",
      age <= 79 ~ "65-79",
      TRUE      ~ "80+"
    ),

    citizenship_group = sample(
      citizenship_groups,
      size = n(),
      replace = TRUE,
      prob = c(
        0.76,
        0.14,
        0.10
      )
    ),

    true_household_id,

    true_address_id = address_id,

    true_region_code = region_code,

    true_municipality_code = municipality_code,

    former_household_id = NA_character_,

    former_address_id = NA_character_,

    former_region_code = NA_character_,

    former_municipality_code = NA_character_,

    departure_date_true = as.Date(NA)
  )


# ---------------------------------------------------------------------
# 7. Generate former residents
# ---------------------------------------------------------------------
# Former residents are no longer part of the true resident population.
# Some of them will nevertheless remain in the synthetic population
# register, thereby creating overcoverage.

former_age_probabilities <- c(
  0.03,
  0.06,
  0.14,
  0.35,
  0.29,
  0.10,
  0.03
)

former_residents <- tibble(
  person_id = sprintf(
    "P%06d",
    (n_true_residents + 1):
      (n_true_residents + n_former_residents)
  ),

  true_resident = 0L,

  former_household_id = sample(
    household_register$household_id,
    size = n_former_residents,
    replace = TRUE
  )
) %>%

  left_join(
    household_register %>%
      select(
        household_id,
        address_id
      ),
    by = c(
      "former_household_id" = "household_id"
    )
  ) %>%

  left_join(
    address_register %>%
      select(
        address_id,
        region_code,
        municipality_code
      ),
    by = "address_id"
  ) %>%

  transmute(
    person_id,

    true_resident,

    sex = sample(
      sex_categories,
      size = n(),
      replace = TRUE,
      prob = c(
        0.50,
        0.50
      )
    ),

    age = sample_age(
      n(),
      probabilities = former_age_probabilities
    ),

    age_group = case_when(
      age <= 5  ~ "0-5",
      age <= 17 ~ "6-17",
      age <= 24 ~ "18-24",
      age <= 39 ~ "25-39",
      age <= 64 ~ "40-64",
      age <= 79 ~ "65-79",
      TRUE      ~ "80+"
    ),

    citizenship_group = sample(
      citizenship_groups,
      size = n(),
      replace = TRUE,
      prob = c(
        0.60,
        0.18,
        0.22
      )
    ),

    true_household_id = NA_character_,

    true_address_id = NA_character_,

    true_region_code = NA_character_,

    true_municipality_code = NA_character_,

    former_household_id,

    former_address_id = address_id,

    former_region_code = region_code,

    former_municipality_code = municipality_code,

    departure_date_true = sample(
      seq.Date(
        from = as.Date("2022-01-01"),
        to = as.Date("2025-11-30"),
        by = "day"
      ),
      size = n(),
      replace = TRUE
    )
  )


# ---------------------------------------------------------------------
# 8. Combine true residents and former residents into hidden truth
# ---------------------------------------------------------------------

synthetic_population_truth <- bind_rows(
  true_residents,
  former_residents
) %>%

  arrange(
    person_id
  )


# ---------------------------------------------------------------------
# 9. Simulate population-register coverage
# ---------------------------------------------------------------------
# True residents can be missing from the population register
# (undercoverage).
#
# Former residents can remain incorrectly registered
# (overcoverage / stale registration).
#
# These probabilities are simulation parameters only.

undercoverage_prob <- 0.02
stale_registration_prob <- 0.80

synthetic_population_truth <- synthetic_population_truth %>%

  mutate(
    register_draw = runif(
      n()
    ),

    in_population_register = if_else(
      true_resident == 1L,
      as.integer(
        register_draw >= undercoverage_prob
      ),
      as.integer(
        register_draw < stale_registration_prob
      )
    ),

    coverage_status_true = case_when(

      true_resident == 1L &
        in_population_register == 1L ~
        "correctly_registered",

      true_resident == 1L &
        in_population_register == 0L ~
        "undercoverage",

      true_resident == 0L &
        in_population_register == 1L ~
        "overcoverage",

      TRUE ~
        "correctly_absent"
    ),

    overcoverage_flag_true = as.integer(
      coverage_status_true == "overcoverage"
    ),

    undercoverage_flag_true = as.integer(
      coverage_status_true == "undercoverage"
    ),

    registered_household_id_sim = if_else(
      true_resident == 1L,
      true_household_id,
      former_household_id
    ),

    registered_address_id_sim = if_else(
      true_resident == 1L,
      true_address_id,
      former_address_id
    ),

    registered_region_code_sim = if_else(
      true_resident == 1L,
      true_region_code,
      former_region_code
    ),

    registered_municipality_code_sim = if_else(
      true_resident == 1L,
      true_municipality_code,
      former_municipality_code
    )
  ) %>%

  select(
    -register_draw
  )


# ---------------------------------------------------------------------
# 10. Derive the observable synthetic population register
# ---------------------------------------------------------------------
# Ground-truth variables are deliberately excluded from the population
# register. They remain available only in synthetic_population_truth.csv
# for later methodological evaluation.

population_register <- synthetic_population_truth %>%

  filter(
    in_population_register == 1L
  ) %>%

  transmute(
    person_id,

    household_id = registered_household_id_sim,

    address_id = registered_address_id_sim,

    region_code = registered_region_code_sim,

    municipality_code = registered_municipality_code_sim,

    sex,

    age,

    age_group,

    citizenship_group,

    registration_status = sample(
      registration_statuses,
      size = n(),
      replace = TRUE,
      prob = c(
        0.93,
        0.07
      )
    ),

    registration_date = sample(
      seq.Date(
        from = as.Date("2005-01-01"),
        to = as.Date("2025-12-31"),
        by = "day"
      ),
      size = n(),
      replace = TRUE
    ),

    last_move_date = sample(
      seq.Date(
        from = as.Date("2010-01-01"),
        to = as.Date("2025-12-31"),
        by = "day"
      ),
      size = n(),
      replace = TRUE
    )
  ) %>%

  mutate(
    last_move_date = if_else(
      last_move_date < registration_date,
      registration_date,
      last_move_date
    )
  )


# ---------------------------------------------------------------------
# 11. Inject imperfections into the population register
# ---------------------------------------------------------------------
# These imperfections mimic generic administrative-data quality issues.
# They are simulation choices rather than empirical estimates.

population_register <- population_register %>%

  mutate(
    citizenship_group = if_else(
      runif(n()) < 0.005,
      NA_character_,
      citizenship_group
    ),

    registration_status = if_else(
      runif(n()) < 0.003,
      NA_character_,
      registration_status
    ),

    last_move_date = if_else(
      runif(n()) < 0.004,
      last_move_date +
        sample(
          1:120,
          size = n(),
          replace = TRUE
        ),
      last_move_date
    ),

    age = if_else(
      runif(n()) < 0.002,
      sample(
        97:110,
        size = n(),
        replace = TRUE
      ),
      age
    ),

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
# 12. Generate employment register independently from synthetic truth
# ---------------------------------------------------------------------
# Administrative employment records are generated from the hidden
# population universe rather than from the population register.
#
# This allows true residents who are missing from the population
# register to appear in employment data.

employment_register <- synthetic_population_truth %>%

  transmute(
    person_id,
    age,
    true_resident
  ) %>%

  mutate(
    eligible_for_employment =
      age >= 18 &
      age <= 67,

    employment_prob = case_when(

      true_resident == 0L ~ 0.05,

      age >= 18 &
        age <= 24 ~ 0.48,

      age >= 25 &
        age <= 39 ~ 0.78,

      age >= 40 &
        age <= 64 ~ 0.73,

      age >= 65 &
        age <= 67 ~ 0.18,

      TRUE ~ 0.02
    ),

    employment_flag = if_else(
      eligible_for_employment &
        runif(n()) < employment_prob,
      1L,
      0L
    ),

    employment_status = case_when(

      employment_flag == 1L ~ sample(
        c(
          "employed",
          "marginal",
          "self_employed"
        ),
        size = n(),
        replace = TRUE,
        prob = c(
          0.78,
          0.12,
          0.10
        )
      ),

      TRUE ~ "no_record"
    ),

    days_employed_last_12m = case_when(

      employment_status == "employed" ~
        pmin(
          365,
          pmax(
            20,
            round(
              rnorm(
                n(),
                mean = 290,
                sd = 60
              )
            )
          )
        ),

      employment_status == "marginal" ~
        pmin(
          250,
          pmax(
            5,
            round(
              rnorm(
                n(),
                mean = 110,
                sd = 40
              )
            )
          )
        ),

      employment_status == "self_employed" ~
        pmin(
          365,
          pmax(
            30,
            round(
              rnorm(
                n(),
                mean = 250,
                sd = 80
              )
            )
          )
        ),

      TRUE ~ 0
    ),

    annual_employment_income = case_when(

      employment_status == "employed" ~
        round(
          rlnorm(
            n(),
            meanlog = 10.2,
            sdlog = 0.45
          ),
          2
        ),

      employment_status == "marginal" ~
        round(
          rlnorm(
            n(),
            meanlog = 8.5,
            sdlog = 0.35
          ),
          2
        ),

      employment_status == "self_employed" ~
        round(
          rlnorm(
            n(),
            meanlog = 10.0,
            sdlog = 0.70
          ),
          2
        ),

      TRUE ~ 0
    ),

    ref_date = reference_date
  ) %>%

  select(
    person_id,
    ref_date,
    employment_status,
    days_employed_last_12m,
    annual_employment_income
  )


# Inject small amounts of missingness and administrative noise.

employment_register <- employment_register %>%

  mutate(
    days_employed_last_12m = if_else(
      runif(n()) < 0.01,
      NA_integer_,
      days_employed_last_12m
    ),

    annual_employment_income = if_else(
      runif(n()) < 0.01,
      NA_real_,
      annual_employment_income
    )
  ) %>%

  filter(
    employment_status != "no_record" |
      runif(n()) < 0.02
  )


# ---------------------------------------------------------------------
# 13. Generate tax register independently from synthetic truth
# ---------------------------------------------------------------------

tax_register <- synthetic_population_truth %>%

  transmute(
    person_id,
    age,
    true_resident
  ) %>%

  mutate(
    tax_filing_prob = case_when(

      true_resident == 0L ~ 0.04,

      age >= 18 &
        age <= 24 ~ 0.22,

      age >= 25 &
        age <= 39 ~ 0.62,

      age >= 40 &
        age <= 64 ~ 0.68,

      age >= 65 &
        age <= 79 ~ 0.28,

      age >= 80 ~ 0.12,

      TRUE ~ 0.01
    ),

    tax_filing_flag = if_else(
      runif(n()) < tax_filing_prob,
      1L,
      0L
    ),

    tax_year = 2025L,

    taxable_income = case_when(

      tax_filing_flag == 1L &
        age >= 18 &
        age <= 24 ~
        round(
          rlnorm(
            n(),
            meanlog = 9.2,
            sdlog = 0.50
          ),
          2
        ),

      tax_filing_flag == 1L &
        age >= 25 &
        age <= 64 ~
        round(
          rlnorm(
            n(),
            meanlog = 10.4,
            sdlog = 0.55
          ),
          2
        ),

      tax_filing_flag == 1L &
        age >= 65 ~
        round(
          rlnorm(
            n(),
            meanlog = 9.5,
            sdlog = 0.45
          ),
          2
        ),

      TRUE ~ 0
    )
  ) %>%

  select(
    person_id,
    tax_year,
    tax_filing_flag,
    taxable_income
  ) %>%

  filter(
    tax_filing_flag == 1L |
      runif(n()) < 0.015
  )


# Inject generic administrative imperfections.

tax_register <- tax_register %>%

  mutate(
    taxable_income = if_else(
      runif(n()) < 0.01,
      NA_real_,
      taxable_income
    ),

    taxable_income = if_else(
      runif(n()) < 0.005,
      -abs(taxable_income),
      taxable_income
    )
  )


# ---------------------------------------------------------------------
# 14. Generate education register independently from synthetic truth
# ---------------------------------------------------------------------

education_register <- synthetic_population_truth %>%

  transmute(
    person_id,
    age,
    true_resident
  ) %>%

  mutate(
    enrolment_prob = case_when(

      true_resident == 0L ~ 0.02,

      age >= 6 &
        age <= 15 ~ 0.96,

      age >= 16 &
        age <= 17 ~ 0.90,

      age >= 18 &
        age <= 24 ~ 0.42,

      age >= 25 &
        age <= 30 ~ 0.08,

      TRUE ~ 0.01
    ),

    enrolment_flag = if_else(
      runif(n()) < enrolment_prob,
      1L,
      0L
    ),

    school_year = "2025/2026",

    institution_type = case_when(

      enrolment_flag == 1L &
        age >= 6 &
        age <= 17 ~
        sample(
          c(
            "school",
            "vocational_school"
          ),
          size = n(),
          replace = TRUE,
          prob = c(
            0.88,
            0.12
          )
        ),

      enrolment_flag == 1L &
        age >= 18 &
        age <= 30 ~
        sample(
          c(
            "university",
            "vocational_school"
          ),
          size = n(),
          replace = TRUE,
          prob = c(
            0.70,
            0.30
          )
        ),

      TRUE ~ NA_character_
    )
  ) %>%

  select(
    person_id,
    school_year,
    enrolment_flag,
    institution_type
  ) %>%

  filter(
    enrolment_flag == 1L
  )


# Inject small amount of missing institution information.

education_register <- education_register %>%

  mutate(
    institution_type = if_else(
      runif(n()) < 0.01,
      NA_character_,
      institution_type
    )
  )


# ---------------------------------------------------------------------
# 15. Add source-specific administrative contact addresses
# ---------------------------------------------------------------------
# Auxiliary administrative sources may contain an address associated
# with the person. This address is treated as source-specific contact
# information, not as proof of current residence.
#
# For most records the administrative contact address corresponds to
# the current address of a true resident or the former address of a
# former resident. A small share contains another address in the same
# region or no usable address.
#
# These probabilities are illustrative simulation parameters.

contact_address_reference_prob <- 0.94
contact_address_alternative_prob <- 0.03


assign_contact_address <- function(
    data,
    truth_data,
    address_data,
    reference_prob,
    alternative_prob
) {

  data %>%

    left_join(
      truth_data %>%
        transmute(
          person_id,

          reference_address_id = if_else(
            true_resident == 1L,
            true_address_id,
            former_address_id
          ),

          reference_region_code = if_else(
            true_resident == 1L,
            true_region_code,
            former_region_code
          )
        ),
      by = "person_id"
    ) %>%

    mutate(
      address_draw = runif(n()),

      alternative_address_id = map2_chr(
        reference_address_id,
        reference_region_code,
        function(reference_id, region_id) {

          if (is.na(region_id)) {
            return(NA_character_)
          }

          candidate_addresses <- address_data$address_id[
            address_data$region_code == region_id &
              address_data$address_id != reference_id
          ]

          if (length(candidate_addresses) == 0) {
            return(NA_character_)
          }

          sample(
            candidate_addresses,
            size = 1
          )
        }
      ),

      contact_address_id = case_when(
        address_draw < reference_prob ~
          reference_address_id,

        address_draw <
          reference_prob +
          alternative_prob ~
          alternative_address_id,

        TRUE ~
          NA_character_
      )
    ) %>%

    select(
      -reference_address_id,
      -reference_region_code,
      -address_draw,
      -alternative_address_id
    )
}


employment_register <- employment_register %>%
  assign_contact_address(
    truth_data = synthetic_population_truth,
    address_data = address_register,
    reference_prob = contact_address_reference_prob,
    alternative_prob = contact_address_alternative_prob
  )


tax_register <- tax_register %>%
  assign_contact_address(
    truth_data = synthetic_population_truth,
    address_data = address_register,
    reference_prob = contact_address_reference_prob,
    alternative_prob = contact_address_alternative_prob
  )


education_register <- education_register %>%
  assign_contact_address(
    truth_data = synthetic_population_truth,
    address_data = address_register,
    reference_prob = contact_address_reference_prob,
    alternative_prob = contact_address_alternative_prob
  )


# ---------------------------------------------------------------------
# 16. Write datasets to disk
# ---------------------------------------------------------------------

write_csv(
  synthetic_population_truth,
  "data/raw/synthetic_population_truth.csv"
)

write_csv(
  address_register,
  "data/raw/address_register.csv"
)

write_csv(
  population_register,
  "data/raw/population_register.csv"
)

write_csv(
  employment_register,
  "data/raw/employment_register.csv"
)

write_csv(
  tax_register,
  "data/raw/tax_register.csv"
)

write_csv(
  education_register,
  "data/raw/education_register.csv"
)


# ---------------------------------------------------------------------
# 17. Print generation summary
# ---------------------------------------------------------------------

n_undercoverage <- sum(
  synthetic_population_truth$coverage_status_true ==
    "undercoverage"
)

n_overcoverage <- sum(
  synthetic_population_truth$coverage_status_true ==
    "overcoverage"
)

n_correctly_registered <- sum(
  synthetic_population_truth$coverage_status_true ==
    "correctly_registered"
)

message(
  "Version 2 synthetic register data generated successfully."
)

message(
  "True residents: ",
  n_true_residents
)

message(
  "Former/non-residents in synthetic universe: ",
  n_former_residents
)

message(
  "Addresses: ",
  nrow(address_register)
)

message(
  "Households: ",
  n_households
)

message(
  "Correctly registered true residents: ",
  n_correctly_registered
)

message(
  "True undercoverage cases: ",
  n_undercoverage
)

message(
  "True overcoverage cases: ",
  n_overcoverage
)

message(
  "Population-register records: ",
  nrow(population_register)
)

message(
  "Employment-register records: ",
  nrow(employment_register)
)

message(
  "Tax-register records: ",
  nrow(tax_register)
)

message(
  "Education-register records: ",
  nrow(education_register)
)

message(
  "Files written to data/raw/"
)