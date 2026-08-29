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
