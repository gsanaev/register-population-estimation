# =====================================================================
# 03_evaluate_education_attainment.R
# Evaluate Synthetic Educational-Attainment Consolidation
# ---------------------------------------------------------------------
# This script evaluates the completed operational 2024 attainment
# output against hidden synthetic education truth.
#
# Hidden truth is used only here, downstream of the operational
# validation, harmonisation, and reconciliation workflows.
#
# Inputs:
#   data/education/processed/education_attainment_2024.csv
#   data/education/raw/synthetic_education_truth.csv
#
# Planned outputs:
#   output/education/tables/education_population_alignment_summary.csv
#   output/education/tables/education_attainment_evaluation_summary.csv
#   output/education/tables/education_evaluation_by_status.csv
#   output/education/tables/education_evaluation_by_decision_reason.csv
# =====================================================================


# ---------------------------------------------------------------------
# 1. Load packages
# ---------------------------------------------------------------------

library(dplyr)
library(readr)


# ---------------------------------------------------------------------
# 2. Define paths
# ---------------------------------------------------------------------

attainment_path <-
  "data/education/processed/education_attainment_2024.csv"

education_truth_path <-
  "data/education/raw/synthetic_education_truth.csv"

education_output_tables_dir <-
  "output/education/tables"


dir.create(
  education_output_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------------------
# 3. Validate required inputs
# ---------------------------------------------------------------------

required_input_files <- c(
  attainment_path,
  education_truth_path
)

missing_input_files <-
  required_input_files[
    !file.exists(
      required_input_files
    )
  ]

if (
  length(
    missing_input_files
  ) > 0L
) {
  stop(
    paste(
      "Required education evaluation inputs are missing:",
      paste(
        missing_input_files,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


attainment_2024 <- read_csv(
  attainment_path,
  show_col_types = FALSE
)

education_truth <- read_csv(
  education_truth_path,
  show_col_types = FALSE
)


assert_required_columns <- function(
  data,
  required_columns,
  data_name
) {

  missing_columns <-
    setdiff(
      required_columns,
      names(data)
    )

  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste(
        data_name,
        "is missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


assert_required_columns(
  attainment_2024,
  c(
    "person_id",
    "attainment_status",
    "attainment_level_min",
    "attainment_level_max",
    "decision_reason"
  ),
  "Operational 2024 attainment output"
)

assert_required_columns(
  education_truth,
  c(
    "person_id",
    "reference_year",
    "age_at_reference_year",
    "true_attainment_level",
    "true_attainment_label"
  ),
  "Synthetic education truth"
)


if (
  anyDuplicated(
    attainment_2024$person_id
  ) > 0L
) {
  stop(
    "Operational 2024 attainment output contains duplicated persons.",
    call. = FALSE
  )
}


message(
  "Education evaluation inputs validated successfully."
)


# ---------------------------------------------------------------------
# 4. Restrict hidden truth to reference year 2024
# ---------------------------------------------------------------------

truth_2024 <-
  education_truth %>%

  filter(
    reference_year == 2024L
  ) %>%

  select(
    person_id,
    age_at_reference_year,
    true_attainment_level,
    true_attainment_label
  )


if (
  anyDuplicated(
    truth_2024$person_id
  ) > 0L
) {
  stop(
    "Synthetic 2024 education truth contains duplicated persons.",
    call. = FALSE
  )
}


if (
  any(
    is.na(
      truth_2024$
        true_attainment_level
    )
  )
) {
  stop(
    "Synthetic 2024 education truth contains missing attainment levels.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 5. Quantify population alignment before attainment evaluation
# ---------------------------------------------------------------------
# Population alignment is evaluated separately from attainment quality.
# Persons outside the overlap are not treated as attainment-classification
# errors because the operational target population and hidden synthetic
# resident population are defined by different processes.

population_overlap <-
  inner_join(
    attainment_2024 %>%
      select(
        person_id
      ),
    truth_2024 %>%
      select(
        person_id
      ),
    by = "person_id"
  )


operational_without_truth <-
  anti_join(
    attainment_2024 %>%
      select(
        person_id
      ),
    truth_2024 %>%
      select(
        person_id
      ),
    by = "person_id"
  )


truth_outside_operational <-
  anti_join(
    truth_2024 %>%
      select(
        person_id
      ),
    attainment_2024 %>%
      select(
        person_id
      ),
    by = "person_id"
  )


education_population_alignment_summary <-
  tibble(
    measure = c(
      "operational_target_persons",
      "hidden_truth_persons",
      "overlap_persons",
      "operational_without_truth",
      "truth_outside_operational"
    ),

    persons = c(
      nrow(
        attainment_2024
      ),
      nrow(
        truth_2024
      ),
      nrow(
        population_overlap
      ),
      nrow(
        operational_without_truth
      ),
      nrow(
        truth_outside_operational
      )
    )
  )


if (
  nrow(
    attainment_2024
  ) == 0L
) {
  stop(
    "Operational 2024 education target population is empty.",
    call. = FALSE
  )
}


if (
  nrow(
    truth_2024
  ) == 0L
) {
  stop(
    "Hidden 2024 education-truth population is empty.",
    call. = FALSE
  )
}


if (
  nrow(
    population_overlap
  ) == 0L
) {
  stop(
    "Operational target and hidden truth have no population overlap.",
    call. = FALSE
  )
}


if (
  nrow(
    population_overlap
  ) +
    nrow(
      operational_without_truth
    ) !=
    nrow(
      attainment_2024
    )
) {
  stop(
    paste(
      "Operational population alignment does not reconcile",
      "to the operational target population."
    ),
    call. = FALSE
  )
}


if (
  nrow(
    population_overlap
  ) +
    nrow(
      truth_outside_operational
    ) !=
    nrow(
      truth_2024
    )
) {
  stop(
    paste(
      "Hidden-truth population alignment does not reconcile",
      "to the 2024 truth population."
    ),
    call. = FALSE
  )
}


message(
  "Education evaluation population alignment established successfully."
)

print(
  education_population_alignment_summary
)


# ---------------------------------------------------------------------
# 6. Join operational attainment states to hidden truth
# ---------------------------------------------------------------------

evaluation_overlap <-
  attainment_2024 %>%

  inner_join(
    truth_2024,
    by = "person_id"
  ) %>%

  mutate(
    has_attainment_range =
      !is.na(
        attainment_level_min
      ) &
      !is.na(
        attainment_level_max
      ),

    range_width =
      if_else(
        has_attainment_range,
        as.numeric(
          attainment_level_max -
            attainment_level_min
        ),
        NA_real_
      ),

    truth_within_range =
      case_when(

        has_attainment_range ~
          true_attainment_level >=
            attainment_level_min &
          true_attainment_level <=
            attainment_level_max,

        TRUE ~
          NA
      ),

    exact_state =
      has_attainment_range &
      attainment_level_min ==
        attainment_level_max,

    exact_match =
      case_when(

        exact_state ~
          true_attainment_level ==
            attainment_level_min,

        TRUE ~
          NA
      )
  )


if (
  nrow(
    evaluation_overlap
  ) !=
    nrow(
      population_overlap
    )
) {
  stop(
    "Attainment evaluation overlap does not match population overlap.",
    call. = FALSE
  )
}


if (
  any(
    xor(
      is.na(
        evaluation_overlap$
          attainment_level_min
      ),
      is.na(
        evaluation_overlap$
          attainment_level_max
      )
    )
  )
) {
  stop(
    "Evaluation data contains partially missing attainment ranges.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 7. Create overall attainment-evaluation summary
# ---------------------------------------------------------------------

range_evaluable_persons <-
  sum(
    evaluation_overlap$
      has_attainment_range
  )

range_agreement_persons <-
  sum(
    evaluation_overlap$
      truth_within_range,
    na.rm = TRUE
  )

exact_state_persons <-
  sum(
    evaluation_overlap$
      exact_state
  )

exact_match_persons <-
  sum(
    evaluation_overlap$
      exact_match,
    na.rm = TRUE
  )


education_attainment_evaluation_summary <-
  tibble(
    overlap_persons =
      nrow(
        evaluation_overlap
      ),

    accepted_persons =
      sum(
        evaluation_overlap$
          attainment_status ==
          "accepted"
      ),

    review_required_persons =
      sum(
        evaluation_overlap$
          attainment_status ==
          "review_required"
      ),

    not_reported_persons =
      sum(
        evaluation_overlap$
          attainment_status ==
          "not_reported"
      ),

    range_evaluable_persons =
      range_evaluable_persons,

    range_coverage_rate =
      range_evaluable_persons /
      nrow(
        evaluation_overlap
      ),

    range_agreement_persons =
      range_agreement_persons,

    range_agreement_rate =
      range_agreement_persons /
      range_evaluable_persons,

    exact_state_persons =
      exact_state_persons,

    exact_match_persons =
      exact_match_persons,

    exact_state_agreement_rate =
      exact_match_persons /
      exact_state_persons,

    broad_range_persons =
      sum(
        evaluation_overlap$
          has_attainment_range &
          !evaluation_overlap$
            exact_state
      ),

    mean_range_width =
      mean(
        evaluation_overlap$
          range_width,
        na.rm = TRUE
      ),

    unresolved_review_without_range =
      sum(
        evaluation_overlap$
          attainment_status ==
          "review_required" &
          !evaluation_overlap$
            has_attainment_range
      )
  )


# ---------------------------------------------------------------------
# 8. Create evaluation summary by final attainment status
# ---------------------------------------------------------------------

education_evaluation_by_status <-
  evaluation_overlap %>%

  group_by(
    attainment_status
  ) %>%

  summarise(
    persons =
      n(),

    persons_with_range =
      sum(
        has_attainment_range
      ),

    range_agreement_persons =
      sum(
        truth_within_range,
        na.rm = TRUE
      ),

    exact_state_persons =
      sum(
        exact_state
      ),

    exact_match_persons =
      sum(
        exact_match,
        na.rm = TRUE
      ),

    mean_range_width =
      mean(
        range_width,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  mutate(
    range_coverage_rate =
      persons_with_range /
      persons,

    range_agreement_rate =
      if_else(
        persons_with_range > 0L,
        range_agreement_persons /
          persons_with_range,
        NA_real_
      ),

    exact_state_agreement_rate =
      if_else(
        exact_state_persons > 0L,
        exact_match_persons /
          exact_state_persons,
        NA_real_
      ),

    mean_range_width =
      if_else(
        persons_with_range > 0L,
        mean_range_width,
        NA_real_
      )
  ) %>%

  arrange(
    factor(
      attainment_status,
      levels = c(
        "accepted",
        "review_required",
        "not_reported"
      )
    )
  )


# ---------------------------------------------------------------------
# 9. Create evaluation summary by reconciliation decision
# ---------------------------------------------------------------------

education_evaluation_by_decision_reason <-
  evaluation_overlap %>%

  group_by(
    attainment_status,
    decision_reason
  ) %>%

  summarise(
    persons =
      n(),

    persons_with_range =
      sum(
        has_attainment_range
      ),

    range_agreement_persons =
      sum(
        truth_within_range,
        na.rm = TRUE
      ),

    exact_state_persons =
      sum(
        exact_state
      ),

    exact_match_persons =
      sum(
        exact_match,
        na.rm = TRUE
      ),

    mean_range_width =
      mean(
        range_width,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  mutate(
    range_coverage_rate =
      persons_with_range /
      persons,

    range_agreement_rate =
      if_else(
        persons_with_range > 0L,
        range_agreement_persons /
          persons_with_range,
        NA_real_
      ),

    exact_state_agreement_rate =
      if_else(
        exact_state_persons > 0L,
        exact_match_persons /
          exact_state_persons,
        NA_real_
      ),

    mean_range_width =
      if_else(
        persons_with_range > 0L,
        mean_range_width,
        NA_real_
      )
  ) %>%

  arrange(
    factor(
      attainment_status,
      levels = c(
        "accepted",
        "review_required",
        "not_reported"
      )
    ),
    desc(
      persons
    ),
    decision_reason
  )


# ---------------------------------------------------------------------
# 10. Validate evaluation summaries
# ---------------------------------------------------------------------

if (
  range_evaluable_persons >
    nrow(
      evaluation_overlap
    )
) {
  stop(
    "Range-evaluable persons exceed the evaluation overlap.",
    call. = FALSE
  )
}


if (
  range_agreement_persons >
    range_evaluable_persons
) {
  stop(
    "Range-agreeing persons exceed range-evaluable persons.",
    call. = FALSE
  )
}


if (
  exact_state_persons >
    range_evaluable_persons
) {
  stop(
    "Exact-state persons exceed range-evaluable persons.",
    call. = FALSE
  )
}


if (
  exact_match_persons >
    exact_state_persons
) {
  stop(
    "Exact matches exceed exact-state persons.",
    call. = FALSE
  )
}


if (
  sum(
    education_evaluation_by_status$
      persons
  ) !=
    nrow(
      evaluation_overlap
    )
) {
  stop(
    "Evaluation-by-status summary does not reconcile to overlap population.",
    call. = FALSE
  )
}


if (
  sum(
    education_evaluation_by_decision_reason$
      persons
  ) !=
    nrow(
      evaluation_overlap
    )
) {
  stop(
    paste(
      "Evaluation-by-decision summary does not reconcile",
      "to overlap population."
    ),
    call. = FALSE
  )
}


if (
  education_attainment_evaluation_summary$
    unresolved_review_without_range >
    education_attainment_evaluation_summary$
      review_required_persons
) {
  stop(
    paste(
      "Unresolved review cases without a range exceed",
      "the total review-required population."
    ),
    call. = FALSE
  )
}


if (
  any(
    evaluation_overlap$
      attainment_status ==
      "not_reported" &
      evaluation_overlap$
        has_attainment_range
  )
) {
  stop(
    "Not-reported evaluation cases unexpectedly contain attainment ranges.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 11. Write evaluation outputs
# ---------------------------------------------------------------------

write_csv(
  education_population_alignment_summary,
  file.path(
    education_output_tables_dir,
    "education_population_alignment_summary.csv"
  )
)


write_csv(
  education_attainment_evaluation_summary,
  file.path(
    education_output_tables_dir,
    "education_attainment_evaluation_summary.csv"
  )
)


write_csv(
  education_evaluation_by_status,
  file.path(
    education_output_tables_dir,
    "education_evaluation_by_status.csv"
  ),
  na = ""
)


write_csv(
  education_evaluation_by_decision_reason,
  file.path(
    education_output_tables_dir,
    "education_evaluation_by_decision_reason.csv"
  ),
  na = ""
)


message(
  "Synthetic education-attainment evaluation completed successfully."
)

message(
  "Evaluation overlap persons: ",
  nrow(
    evaluation_overlap
  )
)

message(
  "Range-evaluable persons: ",
  range_evaluable_persons
)

message(
  "Range agreement rate: ",
  round(
    education_attainment_evaluation_summary$
      range_agreement_rate,
    4
  )
)

message(
  "Exact-state agreement rate: ",
  round(
    education_attainment_evaluation_summary$
      exact_state_agreement_rate,
    4
  )
)
