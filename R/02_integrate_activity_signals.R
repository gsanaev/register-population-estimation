# =====================================================================
# 02_integrate_activity_signals.R
# Integration of Cleaned Registers and Construction of Evidence Features
# Version 2
# ---------------------------------------------------------------------
# This script integrates the cleaned synthetic administrative sources
# into an analysis-ready person-level evidence dataset.
#
# It performs:
#   - integration of cleaned population and auxiliary registers
#   - preservation of source-specific activity and address information
#   - construction of neutral activity-evidence measures
#   - construction of person/address consistency indicators
#   - assignment of an analytical geography without treating it as
#     verified residence
#   - identification of cases that may require later clarification
#   - creation of regional and address-level evidence summaries
#
# Important methodological distinction:
#
#   This script does NOT classify persons as residents or non-residents.
#   It also does NOT use the hidden synthetic ground truth.
#
#   Residence-status estimation and later evaluation against hidden
#   truth belong to subsequent stages.
#
# Outputs:
#   data/processed/person_register_integrated.csv
#   data/processed/region_activity_summary.csv
#   data/processed/address_evidence_summary.csv
# =====================================================================


# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------

library(dplyr)
library(readr)


# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------------------
# 2. Load cleaned datasets
# ---------------------------------------------------------------------

address_clean <- read_csv(
  "data/clean/address_register_clean.csv",
  show_col_types = FALSE
)

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
# 3. Structural checks
# ---------------------------------------------------------------------

message("Running structural checks...")


assert_unique_person_id <- function(
    data,
    dataset_name
) {

  if (
    nrow(data) !=
      n_distinct(data$person_id)
  ) {

    stop(
      paste0(
        "Critical integration error: duplicate person IDs detected in ",
        dataset_name,
        "."
      ),
      call. = FALSE
    )
  }
}


assert_unique_person_id(
  population_clean,
  "population_register_clean.csv"
)

assert_unique_person_id(
  employment_clean,
  "employment_register_clean.csv"
)

assert_unique_person_id(
  tax_clean,
  "tax_register_clean.csv"
)

assert_unique_person_id(
  education_clean,
  "education_register_clean.csv"
)

assert_unique_person_id(
  activity_summary,
  "register_activity_summary.csv"
)


# ---------------------------------------------------------------------
# 4. Guard against hidden-truth leakage
# ---------------------------------------------------------------------

forbidden_truth_columns <- c(
  "resident_true",
  "true_resident",
  "coverage_status_true",
  "true_address_id",
  "former_address_id",
  "undercoverage_flag_true",
  "overcoverage_flag_true"
)

truth_columns_found <- intersect(
  forbidden_truth_columns,
  names(activity_summary)
)

if (
  length(truth_columns_found) > 0
) {

  stop(
    paste0(
      "Critical integration error: hidden ground-truth columns found in ",
      "register_activity_summary.csv: ",
      paste(
        truth_columns_found,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 5. Prepare compact source-specific analytical variables
# ---------------------------------------------------------------------

message("Preparing source-specific analytical variables...")


population_details <- population_clean %>%

  transmute(
    person_id,

    registration_date =
      registration_date,

    last_move_date =
      last_move_date
  )


employment_details <- employment_clean %>%

  transmute(
    person_id,

    employment_status =
      employment_status,

    employment_days_last_12m =
      as.numeric(
        days_employed_last_12m
      ),

    employment_annual_income =
      as.numeric(
        annual_employment_income
      ),

    employment_ref_date =
      ref_date
  )


tax_details <- tax_clean %>%

  transmute(
    person_id,

    tax_year =
      tax_year,

    tax_filing_flag =
      tax_filing_flag,

    taxable_income =
      as.numeric(
        taxable_income
      )
  )


education_details <- education_clean %>%

  transmute(
    person_id,

    enrolment_flag =
      enrolment_flag,

    institution_type =
      institution_type
  )


# ---------------------------------------------------------------------
# 6. Integrate cleaned register evidence
# ---------------------------------------------------------------------

message("Integrating cleaned register evidence...")


person_register_integrated <- activity_summary %>%

  left_join(
    population_details,
    by = "person_id"
  ) %>%

  left_join(
    employment_details,
    by = "person_id"
  ) %>%

  left_join(
    tax_details,
    by = "person_id"
  ) %>%

  left_join(
    education_details,
    by = "person_id"
  ) %>%

  mutate(
    # ---------------------------------------------------------------
    # Activity evidence
    # ---------------------------------------------------------------

    activity_evidence_level = case_when(

      n_activity_signals >= 2L ~
        "multiple_positive_signals",

      n_activity_signals == 1L ~
        "single_positive_signal",

      TRUE ~
        "no_positive_signal"
    ),

    economic_activity_flag =
      as.integer(
        employment_signal == 1L |
        tax_signal == 1L
      ),

    education_activity_flag =
      as.integer(
        education_signal == 1L
      ),

    # ---------------------------------------------------------------
    # Address evidence
    # ---------------------------------------------------------------

    address_evidence_status = case_when(

      in_population_register == 1L &
        n_contact_addresses_available == 0L ~
        "registered_no_auxiliary_address",

      in_population_register == 1L &
        flag_population_auxiliary_address_disagreement ~
        "registered_auxiliary_addresses_differ",

      in_population_register == 1L &
        flag_auxiliary_address_conflict &
        n_contact_addresses_matching_population > 0L ~
        "registered_mixed_auxiliary_addresses",

      in_population_register == 1L &
        n_contact_addresses_matching_population > 0L ~
        "registered_address_supported",

      in_population_register == 0L &
        n_contact_addresses_available == 0L ~
        "auxiliary_only_no_address",

      in_population_register == 0L &
        flag_auxiliary_address_conflict ~
        "auxiliary_only_conflicting_addresses",

      in_population_register == 0L &
        !is.na(
          consistent_auxiliary_address_id
        ) ~
        "auxiliary_only_consistent_address",

      TRUE ~
        "address_evidence_unresolved"
    ),

    # ---------------------------------------------------------------
    # Analytical geography
    # ---------------------------------------------------------------
    # For persons in the population register, analytical geography
    # comes from the registered address.
    #
    # For auxiliary-only persons, geography is assigned only where
    # auxiliary sources provide one consistent address.
    #
    # This is an analytical location for evidence aggregation, not a
    # verified statement of residence.

    analysis_address_id = case_when(

      in_population_register == 1L ~
        address_id,

      in_population_register == 0L &
        !is.na(
          consistent_auxiliary_address_id
        ) ~
        consistent_auxiliary_address_id,

      TRUE ~
        NA_character_
    ),

    analysis_region_code = case_when(

      in_population_register == 1L ~
        region_code,

      in_population_register == 0L &
        !is.na(
          consistent_auxiliary_region_code
        ) ~
        consistent_auxiliary_region_code,

      TRUE ~
        NA_character_
    ),

    analysis_municipality_code = case_when(

      in_population_register == 1L ~
        municipality_code,

      in_population_register == 0L &
        !is.na(
          consistent_auxiliary_municipality_code
        ) ~
        consistent_auxiliary_municipality_code,

      TRUE ~
        NA_character_
    ),

    analysis_geography_source = case_when(

      in_population_register == 1L ~
        "population_register",

      in_population_register == 0L &
        !is.na(
          consistent_auxiliary_address_id
        ) ~
        "consistent_auxiliary_address",

      TRUE ~
        "unresolved"
    ),

    flag_geography_unresolved =
      is.na(
        analysis_address_id
      ),

    # ---------------------------------------------------------------
    # Observed evidence case
    # ---------------------------------------------------------------
    # These groups describe what is observed. They do not represent a
    # residence-status decision.

    observed_evidence_case = case_when(

      in_population_register == 1L &
        n_activity_signals > 0L &
        flag_population_auxiliary_address_disagreement ~
        "registered_activity_address_disagreement",

      in_population_register == 1L &
        n_activity_signals > 0L ~
        "registered_with_positive_activity",

      in_population_register == 1L &
        n_activity_signals == 0L &
        n_contact_addresses_available > 0L ~
        "registered_no_activity_with_auxiliary_address",

      in_population_register == 1L ~
        "registered_no_activity_no_auxiliary_address",

      in_population_register == 0L &
        n_activity_signals > 0L &
        !is.na(
          consistent_auxiliary_address_id
        ) ~
        "auxiliary_only_activity_consistent_address",

      in_population_register == 0L &
        n_activity_signals > 0L ~
        "auxiliary_only_activity_unresolved_address",

      TRUE ~
        "auxiliary_only_without_positive_activity"
    ),

    # ---------------------------------------------------------------
    # Cases for a later synthetic clarification step
    # ---------------------------------------------------------------
    # "Clarification" here refers only to this synthetic methodological
    # workflow. It is not intended to reproduce an official Destatis
    # survey or administrative procedure.

    clarification_reason = case_when(

      in_population_register == 0L &
        flag_auxiliary_address_conflict ~
        "auxiliary_only_conflicting_addresses",

      in_population_register == 0L &
        is.na(
          consistent_auxiliary_address_id
        ) ~
        "auxiliary_only_without_consistent_address",

      in_population_register == 0L ~
        "auxiliary_only_consistent_address",

      in_population_register == 1L &
        flag_population_auxiliary_address_disagreement ~
        "registered_auxiliary_address_disagreement",

      in_population_register == 1L &
        flag_auxiliary_address_conflict ~
        "registered_mixed_auxiliary_addresses",

      TRUE ~
        "none"
    ),

    flag_clarification_case =
      clarification_reason != "none"
  ) %>%

  arrange(
    person_id
  )


# ---------------------------------------------------------------------
# 7. Post-integration validation
# ---------------------------------------------------------------------

message("Running post-integration validation...")


if (
  nrow(person_register_integrated) !=
    n_distinct(
      person_register_integrated$person_id
    )
) {

  stop(
    "Critical integration error: duplicate person IDs after integration.",
    call. = FALSE
  )
}


if (
  any(
    person_register_integrated$
      n_activity_signals < 0L |
      person_register_integrated$
        n_activity_signals > 3L,
    na.rm = TRUE
  )
) {

  stop(
    "Critical integration error: invalid n_activity_signals values.",
    call. = FALSE
  )
}


integrated_truth_columns <- intersect(
  forbidden_truth_columns,
  names(
    person_register_integrated
  )
)

if (
  length(
    integrated_truth_columns
  ) > 0
) {

  stop(
    paste0(
      "Critical integration error: hidden truth leaked into integrated output: ",
      paste(
        integrated_truth_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


invalid_analysis_addresses <- person_register_integrated %>%

  filter(
    !is.na(
      analysis_address_id
    ),
    !analysis_address_id %in%
      address_clean$address_id
  )


if (
  nrow(
    invalid_analysis_addresses
  ) > 0
) {

  stop(
    paste0(
      "Critical integration error: ",
      nrow(
        invalid_analysis_addresses
      ),
      " analytical address IDs are not found in the cleaned address register."
    ),
    call. = FALSE
  )
}


message(
  "Observed persons = ",
  nrow(
    person_register_integrated
  )
)

message(
  "Persons in population register = ",
  sum(
    person_register_integrated$
      in_population_register == 1L,
    na.rm = TRUE
  )
)

message(
  "Auxiliary-only persons = ",
  sum(
    person_register_integrated$
      flag_auxiliary_only,
    na.rm = TRUE
  )
)

message(
  "Persons with unresolved analytical geography = ",
  sum(
    person_register_integrated$
      flag_geography_unresolved,
    na.rm = TRUE
  )
)

message(
  "Cases marked for later clarification = ",
  sum(
    person_register_integrated$
      flag_clarification_case,
    na.rm = TRUE
  )
)


message("Observed evidence-case distribution:")

print(
  person_register_integrated %>%

    count(
      observed_evidence_case,
      sort = TRUE
    )
)


message("Address-evidence distribution:")

print(
  person_register_integrated %>%

    count(
      address_evidence_status,
      sort = TRUE
    )
)


# ---------------------------------------------------------------------
# 8. Create regional evidence summary
# ---------------------------------------------------------------------

message("Building regional evidence summary...")


region_activity_summary <- person_register_integrated %>%

  mutate(
    summary_region_code =
      coalesce(
        analysis_region_code,
        "UNRESOLVED"
      )
  ) %>%

  group_by(
    summary_region_code
  ) %>%

  summarise(
    observed_persons =
      n(),

    registered_persons =
      sum(
        in_population_register == 1L,
        na.rm = TRUE
      ),

    auxiliary_only_persons =
      sum(
        flag_auxiliary_only,
        na.rm = TRUE
      ),

    auxiliary_only_with_activity =
      sum(
        flag_auxiliary_only_with_activity,
        na.rm = TRUE
      ),

    registered_without_positive_activity =
      sum(
        flag_registered_without_activity,
        na.rm = TRUE
      ),

    persons_with_multiple_positive_signals =
      sum(
        n_activity_signals >= 2L,
        na.rm = TRUE
      ),

    auxiliary_address_conflict_cases =
      sum(
        flag_auxiliary_address_conflict,
        na.rm = TRUE
      ),

    population_auxiliary_address_disagreement_cases =
      sum(
        flag_population_auxiliary_address_disagreement,
        na.rm = TRUE
      ),

    clarification_cases =
      sum(
        flag_clarification_case,
        na.rm = TRUE
      ),

    geography_unresolved_cases =
      sum(
        flag_geography_unresolved,
        na.rm = TRUE
      ),

    mean_activity_signals =
      mean(
        n_activity_signals,
        na.rm = TRUE
      ),

    employment_signal_rate =
      mean(
        employment_signal,
        na.rm = TRUE
      ),

    tax_signal_rate =
      mean(
        tax_signal,
        na.rm = TRUE
      ),

    education_signal_rate =
      mean(
        education_signal,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  arrange(
    summary_region_code
  )


# ---------------------------------------------------------------------
# 9. Create address-level evidence summary
# ---------------------------------------------------------------------

message("Building address-level evidence summary...")


address_evidence_summary <- person_register_integrated %>%

  filter(
    !is.na(
      analysis_address_id
    )
  ) %>%

  group_by(
    analysis_address_id,
    analysis_region_code,
    analysis_municipality_code
  ) %>%

  summarise(
    observed_persons =
      n(),

    registered_persons =
      sum(
        in_population_register == 1L,
        na.rm = TRUE
      ),

    auxiliary_only_persons =
      sum(
        flag_auxiliary_only,
        na.rm = TRUE
      ),

    persons_with_positive_activity =
      sum(
        n_activity_signals > 0L,
        na.rm = TRUE
      ),

    persons_with_multiple_positive_signals =
      sum(
        n_activity_signals >= 2L,
        na.rm = TRUE
      ),

    persons_with_address_conflict =
      sum(
        flag_auxiliary_address_conflict,
        na.rm = TRUE
      ),

    clarification_cases =
      sum(
        flag_clarification_case,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  arrange(
    analysis_region_code,
    analysis_municipality_code,
    analysis_address_id
  )


# ---------------------------------------------------------------------
# 10. Write processed datasets
# ---------------------------------------------------------------------

write_csv(
  person_register_integrated,
  "data/processed/person_register_integrated.csv"
)

write_csv(
  region_activity_summary,
  "data/processed/region_activity_summary.csv"
)

write_csv(
  address_evidence_summary,
  "data/processed/address_evidence_summary.csv"
)


message(
  "Evidence integration completed successfully."
)

message(
  "Files written to data/processed/"
)