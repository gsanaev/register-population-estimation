# =====================================================================
# 01_generate_education_evidence.R
# Generate Synthetic Educational-Attainment Evidence
# ---------------------------------------------------------------------
# This optional workflow extends the synthetic register-population
# project with longitudinal educational-attainment evidence.
#
# The generated education truth, source deliveries, code systems, and
# simulation parameters are illustrative. They do not reproduce official
# Destatis source schemas or production procedures.
#
# Inputs:
#   data/raw/synthetic_population_truth.csv
#
# Outputs:
#   data/education/raw/
# =====================================================================


# ---------------------------------------------------------------------
# 1. Load packages and set reproducible seed
# ---------------------------------------------------------------------

library(dplyr)
library(readr)

education_seed <- 2027L

set.seed(
  education_seed
)


# ---------------------------------------------------------------------
# 2. Define paths and ensure output directory exists
# ---------------------------------------------------------------------

population_truth_path <-
  "data/raw/synthetic_population_truth.csv"

education_raw_dir <-
  "data/education/raw"

dir.create(
  education_raw_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------------------
# 3. Load and validate the synthetic population universe
# ---------------------------------------------------------------------

if (!file.exists(population_truth_path)) {
  stop(
    "Synthetic population truth file is missing.",
    call. = FALSE
  )
}

synthetic_population_truth <- read_csv(
  population_truth_path,
  show_col_types = FALSE
)

required_population_columns <- c(
  "person_id",
  "true_resident",
  "age"
)

missing_population_columns <- setdiff(
  required_population_columns,
  names(synthetic_population_truth)
)

if (length(missing_population_columns) > 0L) {
  stop(
    paste(
      "Synthetic population truth is missing required columns:",
      paste(
        missing_population_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (anyDuplicated(synthetic_population_truth$person_id) > 0L) {
  stop(
    "Synthetic population truth contains duplicated person_id values.",
    call. = FALSE
  )
}

if (
  any(
    !synthetic_population_truth$true_resident %in% c(
      0L,
      1L
    )
  )
) {
  stop(
    "true_resident must contain only 0 and 1.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      synthetic_population_truth$person_id
    )
  )
) {
  stop(
    "Synthetic population truth contains missing person_id values.",
    call. = FALSE
  )
}

message(
  "Education evidence generator input validated successfully."
)


# ---------------------------------------------------------------------
# 4. Define the synthetic attainment taxonomy
# ---------------------------------------------------------------------

attainment_labels <- c(
  "low_or_none",
  "school_qualification",
  "vocational_or_postsecondary",
  "bachelor_or_equivalent",
  "master_or_equivalent",
  "doctorate"
)

minimum_age_by_level <- c(
  15L,
  15L,
  16L,
  20L,
  22L,
  25L
)


attainment_label <- function(level) {

  if (
    any(
      !level %in% seq_along(
        attainment_labels
      )
    )
  ) {
    stop(
      "Invalid attainment level.",
      call. = FALSE
    )
  }

  attainment_labels[level]
}


# ---------------------------------------------------------------------
# 5. Define illustrative attainment-generation functions
# ---------------------------------------------------------------------
# The probabilities and minimum-age thresholds below are simulation
# parameters. They create internally plausible heterogeneity by age;
# they are not estimates of German educational-attainment distributions
# or official qualification-age rules.

base_attainment_probabilities <- function(age_value) {

  if (age_value <= 17L) {
    return(
      c(
        0.35,
        0.62,
        0.03,
        0.00,
        0.00,
        0.00
      )
    )
  }

  if (age_value <= 24L) {
    return(
      c(
        0.08,
        0.42,
        0.30,
        0.18,
        0.02,
        0.00
      )
    )
  }

  if (age_value <= 39L) {
    return(
      c(
        0.05,
        0.18,
        0.40,
        0.24,
        0.11,
        0.02
      )
    )
  }

  if (age_value <= 64L) {
    return(
      c(
        0.08,
        0.22,
        0.45,
        0.15,
        0.08,
        0.02
      )
    )
  }

  c(
    0.15,
    0.30,
    0.38,
    0.10,
    0.05,
    0.02
  )
}


sample_attainment_by_age <- function(age_values) {

  if (
    any(
      is.na(age_values) |
        age_values < 15L
    )
  ) {
    stop(
      "Attainment generation requires non-missing ages of 15 or older.",
      call. = FALSE
    )
  }

  vapply(
    age_values,
    function(age_value) {

      probabilities <-
        base_attainment_probabilities(
          age_value
        )

      allowed_levels <-
        age_value >=
        minimum_age_by_level

      probabilities[
        !allowed_levels
      ] <- 0

      probabilities <-
        probabilities /
        sum(probabilities)

      sample(
        1:6,
        size = 1L,
        prob = probabilities
      )
    },
    integer(1)
  )
}


progress_attainment <- function(
  current_level,
  age_at_start,
  age_at_end
) {

  progression_probability <- case_when(
    age_at_start >= 15 &
      age_at_start <= 17 ~ 0.65,

    age_at_start >= 18 &
      age_at_start <= 24 ~ 0.45,

    age_at_start >= 25 &
      age_at_start <= 30 ~ 0.15,

    age_at_start >= 31 &
      age_at_start <= 39 ~ 0.06,

    TRUE ~ 0.015
  )

  next_level <-
    pmin(
      current_level + 1L,
      6L
    )

  can_progress <-
    current_level < 6L &
    age_at_end >=
      minimum_age_by_level[
        next_level
      ]

  progression_draw <-
    runif(
      length(current_level)
    )

  if_else(
    can_progress &
      progression_draw <
        progression_probability,
    next_level,
    current_level
  )
}


# ---------------------------------------------------------------------
# 6. Build hidden synthetic educational-attainment truth
# ---------------------------------------------------------------------

education_population <- synthetic_population_truth %>%

  filter(
    true_resident == 1L
  ) %>%

  transmute(
    person_id,
    age_2022 = age - 3L,
    age_2024 = age - 1L
  )


education_truth_2022 <- education_population %>%

  filter(
    age_2022 >= 15L
  ) %>%

  mutate(
    true_attainment_level =
      sample_attainment_by_age(
        age_2022
      ),

    true_attainment_label =
      attainment_label(
        true_attainment_level
      )
  ) %>%

  transmute(
    person_id,
    reference_year = 2022L,
    age_at_reference_year = age_2022,
    true_attainment_level,
    true_attainment_label
  )


education_truth_2024_existing <-
  education_truth_2022 %>%

  left_join(
    education_population %>%
      select(
        person_id,
        age_2024
      ),
    by = "person_id"
  ) %>%

  mutate(
    true_attainment_level_2024 =
      progress_attainment(
        true_attainment_level,
        age_at_reference_year,
        age_2024
      ),

    true_attainment_label_2024 =
      attainment_label(
        true_attainment_level_2024
      )
  ) %>%

  transmute(
    person_id,
    reference_year = 2024L,
    age_at_reference_year = age_2024,
    true_attainment_level =
      true_attainment_level_2024,
    true_attainment_label =
      true_attainment_label_2024
  )


education_truth_2024_new <-
  education_population %>%

  anti_join(
    education_truth_2022 %>%
      select(
        person_id
      ),
    by = "person_id"
  ) %>%

  filter(
    age_2024 >= 15L
  ) %>%

  mutate(
    true_attainment_level =
      sample_attainment_by_age(
        age_2024
      ),

    true_attainment_label =
      attainment_label(
        true_attainment_level
      )
  ) %>%

  transmute(
    person_id,
    reference_year = 2024L,
    age_at_reference_year = age_2024,
    true_attainment_level,
    true_attainment_label
  )


synthetic_education_truth <- bind_rows(
  education_truth_2022,
  education_truth_2024_existing,
  education_truth_2024_new
) %>%

  arrange(
    person_id,
    reference_year
  )


# ---------------------------------------------------------------------
# 7. Validate and write hidden education truth
# ---------------------------------------------------------------------

expected_truth_rows <-
  nrow(education_truth_2022) +
  nrow(education_truth_2024_existing) +
  nrow(education_truth_2024_new)

if (
  nrow(synthetic_education_truth) !=
    expected_truth_rows
) {
  stop(
    "Unexpected number of education truth rows.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    synthetic_education_truth[
      c(
        "person_id",
        "reference_year"
      )
    ]
  ) > 0L
) {
  stop(
    paste(
      "Education truth contains duplicated",
      "person-year records."
    ),
    call. = FALSE
  )
}

if (
  any(
    synthetic_education_truth$
      age_at_reference_year < 15L
  )
) {
  stop(
    "Education truth contains persons younger than 15.",
    call. = FALSE
  )
}

minimum_allowed_age <-
  minimum_age_by_level[
    synthetic_education_truth$
      true_attainment_level
  ]

if (
  any(
    synthetic_education_truth$
      age_at_reference_year <
      minimum_allowed_age
  )
) {
  stop(
    paste(
      "Education truth contains age-attainment combinations",
      "below the synthetic plausibility thresholds."
    ),
    call. = FALSE
  )
}

if (
  any(
    !synthetic_education_truth$
      true_attainment_level %in% 1:6
  )
) {
  stop(
    "Education truth contains invalid attainment levels.",
    call. = FALSE
  )
}


truth_progression_check <-
  education_truth_2022 %>%

  select(
    person_id,
    level_2022 = true_attainment_level
  ) %>%

  inner_join(
    education_truth_2024_existing %>%
      select(
        person_id,
        level_2024 = true_attainment_level
      ),
    by = "person_id"
  )

if (
  any(
    truth_progression_check$
      level_2024 <
      truth_progression_check$
        level_2022
  )
) {
  stop(
    "Hidden education truth contains downward attainment progression.",
    call. = FALSE
  )
}


write_csv(
  synthetic_education_truth,
  file.path(
    education_raw_dir,
    "synthetic_education_truth.csv"
  )
)


n_true_progressions <- sum(
  truth_progression_check$
    level_2024 >
    truth_progression_check$
      level_2022
)

message(
  "Hidden synthetic education truth generated successfully."
)

message(
  "2022 eligible persons: ",
  nrow(
    education_truth_2022
  )
)

message(
  "2024 eligible persons: ",
  nrow(
    education_truth_2024_existing
  ) +
    nrow(
      education_truth_2024_new
    )
)

message(
  "Newly eligible in 2024: ",
  nrow(
    education_truth_2024_new
  )
)

message(
  "True upward progressions 2022-2024: ",
  n_true_progressions
)


# ---------------------------------------------------------------------
# 8. Define Zensus-like 2022 delivery parameters
# ---------------------------------------------------------------------
# The source coverage and error rates are illustrative simulation
# parameters. They are not estimates of actual administrative or
# statistical data quality.

zensus_2022_seed <- 20221L

zensus_2022_coverage <- 0.20

zensus_2022_measurement_error_rate <- 0.015
zensus_2022_missing_qualification_rate <- 0.010
zensus_2022_unknown_code_rate <- 0.004
zensus_2022_invalid_year_rate <- 0.002
zensus_2022_missing_person_id_rate <- 0.002
zensus_2022_duplicate_rate <- 0.005

zensus_2022_codes <- c(
  "NONE_LOW",
  "SCHOOL",
  "VOC_POSTSEC",
  "BACHELOR_EQ",
  "MASTER_EQ",
  "DOCTORATE"
)


# ---------------------------------------------------------------------
# 9. Generate Zensus-like 2022 source evidence
# ---------------------------------------------------------------------

set.seed(
  zensus_2022_seed
)

n_zensus_2022_base <-
  floor(
    nrow(education_truth_2022) *
      zensus_2022_coverage
  )


zensus_2022_working <-
  education_truth_2022 %>%

  slice_sample(
    n = n_zensus_2022_base
  ) %>%

  mutate(
    observed_attainment_level =
      true_attainment_level
  )


sample_alternative_attainment <- function(
  true_level,
  age_value
) {

  allowed_levels <-
    which(
      age_value >=
        minimum_age_by_level
    )

  candidate_levels <-
    allowed_levels[
      allowed_levels !=
        true_level
    ]

  if (length(candidate_levels) == 0L) {
    stop(
      "No alternative attainment level available.",
      call. = FALSE
    )
  }

  distance_from_truth <-
    abs(
      candidate_levels -
        true_level
    )

  nearest_candidates <-
    candidate_levels[
      distance_from_truth ==
        min(distance_from_truth)
    ]

  nearest_candidates[
    sample.int(
      length(nearest_candidates),
      size = 1L
    )
  ]
}


n_measurement_error <-
  floor(
    n_zensus_2022_base *
      zensus_2022_measurement_error_rate
  )

n_missing_qualification <-
  floor(
    n_zensus_2022_base *
      zensus_2022_missing_qualification_rate
  )

n_unknown_code <-
  floor(
    n_zensus_2022_base *
      zensus_2022_unknown_code_rate
  )

n_invalid_year <-
  floor(
    n_zensus_2022_base *
      zensus_2022_invalid_year_rate
  )

n_missing_person_id <-
  floor(
    n_zensus_2022_base *
      zensus_2022_missing_person_id_rate
  )

n_duplicate_records <-
  floor(
    n_zensus_2022_base *
      zensus_2022_duplicate_rate
  )


available_indices <-
  seq_len(
    n_zensus_2022_base
  )


measurement_error_indices <-
  sample(
    available_indices,
    size = n_measurement_error
  )

available_indices <-
  setdiff(
    available_indices,
    measurement_error_indices
  )


missing_qualification_indices <-
  sample(
    available_indices,
    size = n_missing_qualification
  )

available_indices <-
  setdiff(
    available_indices,
    missing_qualification_indices
  )


unknown_code_indices <-
  sample(
    available_indices,
    size = n_unknown_code
  )

available_indices <-
  setdiff(
    available_indices,
    unknown_code_indices
  )


invalid_year_indices <-
  sample(
    available_indices,
    size = n_invalid_year
  )

available_indices <-
  setdiff(
    available_indices,
    invalid_year_indices
  )


missing_person_id_indices <-
  sample(
    available_indices,
    size = n_missing_person_id
  )

available_indices <-
  setdiff(
    available_indices,
    missing_person_id_indices
  )


duplicate_indices <-
  sample(
    available_indices,
    size = n_duplicate_records
  )


for (index in measurement_error_indices) {

  zensus_2022_working$
    observed_attainment_level[index] <-
    sample_alternative_attainment(
      true_level =
        zensus_2022_working$
          true_attainment_level[index],
      age_value =
        zensus_2022_working$
          age_at_reference_year[index]
    )
}


realised_measurement_errors <-
  sum(
    zensus_2022_working$
      observed_attainment_level !=
      zensus_2022_working$
        true_attainment_level
  )

if (
  realised_measurement_errors !=
    n_measurement_error
) {
  stop(
    "Unexpected number of realised Zensus-like measurement errors.",
    call. = FALSE
  )
}


zensus_2022_delivery <-
  zensus_2022_working %>%

  mutate(
    highest_qualification =
      zensus_2022_codes[
        observed_attainment_level
      ]
  )


zensus_2022_delivery$
  highest_qualification[
    missing_qualification_indices
  ] <- NA_character_

zensus_2022_delivery$
  highest_qualification[
    unknown_code_indices
  ] <- "UNKNOWN_CODE"

zensus_2022_delivery$
  reference_year[
    invalid_year_indices
  ] <- 2021L

zensus_2022_delivery$
  person_id[
    missing_person_id_indices
  ] <- NA_character_


zensus_2022_duplicates <-
  zensus_2022_delivery[
    duplicate_indices,
    ,
    drop = FALSE
  ]


zensus_2022_delivery <-
  bind_rows(
    zensus_2022_delivery,
    zensus_2022_duplicates
  ) %>%

  select(
    person_id,
    reference_year,
    highest_qualification
  )


# ---------------------------------------------------------------------
# 10. Validate and write Zensus-like 2022 delivery
# ---------------------------------------------------------------------

expected_zensus_rows <-
  n_zensus_2022_base +
  n_duplicate_records

if (
  nrow(zensus_2022_delivery) !=
    expected_zensus_rows
) {
  stop(
    "Unexpected number of Zensus-like delivery rows.",
    call. = FALSE
  )
}

if (
  sum(
    is.na(
      zensus_2022_delivery$
        person_id
    )
  ) !=
    n_missing_person_id
) {
  stop(
    "Unexpected number of missing person_id values.",
    call. = FALSE
  )
}

if (
  sum(
    is.na(
      zensus_2022_delivery$
        highest_qualification
    )
  ) !=
    n_missing_qualification
) {
  stop(
    "Unexpected number of missing qualification values.",
    call. = FALSE
  )
}

if (
  sum(
    zensus_2022_delivery$
      highest_qualification ==
      "UNKNOWN_CODE",
    na.rm = TRUE
  ) !=
    n_unknown_code
) {
  stop(
    "Unexpected number of unknown qualification codes.",
    call. = FALSE
  )
}

if (
  sum(
    zensus_2022_delivery$
      reference_year != 2022L
  ) !=
    n_invalid_year
) {
  stop(
    "Unexpected number of invalid reference years.",
    call. = FALSE
  )
}


zensus_duplicate_keys <-
  zensus_2022_delivery %>%

  filter(
    !is.na(person_id)
  ) %>%

  count(
    person_id,
    reference_year
  ) %>%

  filter(
    n > 1L
  )

if (
  nrow(zensus_duplicate_keys) !=
    n_duplicate_records
) {
  stop(
    paste(
      "Unexpected number of duplicated",
      "Zensus-like person-year keys."
    ),
    call. = FALSE
  )
}


write_csv(
  zensus_2022_delivery,
  file.path(
    education_raw_dir,
    "zensus_2022_like_delivery.csv"
  ),
  na = ""
)


message(
  "Zensus-like 2022 delivery generated successfully."
)

message(
  "Base source records: ",
  n_zensus_2022_base
)

message(
  "Measurement errors: ",
  n_measurement_error
)

message(
  "Missing qualifications: ",
  n_missing_qualification
)

message(
  "Unknown codes: ",
  n_unknown_code
)

message(
  "Invalid reference years: ",
  n_invalid_year
)

message(
  "Missing person IDs: ",
  n_missing_person_id
)

message(
  "Duplicate records added: ",
  n_duplicate_records
)

message(
  "Raw delivery rows: ",
  nrow(
    zensus_2022_delivery
  )
)
