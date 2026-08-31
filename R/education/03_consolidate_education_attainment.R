# =====================================================================
# 03_consolidate_education_attainment.R
# Consolidate Synthetic Educational-Attainment Evidence
# ---------------------------------------------------------------------
# This workflow integrates harmonised person-level education evidence
# across sources and reference years.
#
# Operational population scope is derived only from observable register
# evidence. Hidden synthetic education truth is deliberately excluded
# from this workflow and is reserved for later evaluation.
#
# Inputs:
#   data/processed/person_population_estimate.csv
#   data/education/clean/education_evidence_usable.csv
#
# Outputs:
#   data/education/processed/education_attainment_person_year.csv
#   data/education/processed/education_attainment_2024.csv
#   output/education/tables/education_population_scope_summary.csv
#   output/education/tables/education_reconciliation_summary.csv
# =====================================================================


# ---------------------------------------------------------------------
# 1. Load packages and helper functions
# ---------------------------------------------------------------------

library(dplyr)
library(readr)

source(
  "R/education/education_helpers.R"
)


# ---------------------------------------------------------------------
# 2. Define paths
# ---------------------------------------------------------------------

population_path <-
  "data/processed/person_population_estimate.csv"

education_evidence_path <-
  "data/education/clean/education_evidence_usable.csv"

education_processed_dir <-
  "data/education/processed"

education_output_tables_dir <-
  "output/education/tables"


dir.create(
  education_processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  education_output_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------------------
# 3. Validate required inputs
# ---------------------------------------------------------------------

required_input_files <- c(
  population_path,
  education_evidence_path
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
      "Required consolidation inputs are missing:",
      paste(
        missing_input_files,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


population <- read_csv(
  population_path,
  show_col_types = FALSE
)

education_evidence <- read_csv(
  education_evidence_path,
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
  population,
  c(
    "person_id",
    "age",
    "evidence_fallback_resident"
  ),
  "Person population estimate"
)

assert_required_columns(
  education_evidence,
  c(
    "source",
    "source_row_id",
    "person_id",
    "reference_year",
    "source_code",
    "level_min",
    "level_max",
    "qa_status",
    "qa_reason"
  ),
  "Usable education evidence"
)


if (
  anyDuplicated(
    population$person_id
  ) > 0L
) {
  stop(
    "Person population estimate contains duplicated person_id values.",
    call. = FALSE
  )
}


if (
  any(
    education_evidence$
      qa_status != "usable"
  )
) {
  stop(
    paste(
      "Education consolidation input contains",
      "non-usable evidence records."
    ),
    call. = FALSE
  )
}


for (
  observation_index in
    seq_len(
      nrow(
        education_evidence
      )
    )
) {

  assert_valid_attainment_range(
    education_evidence$
      level_min[
        observation_index
      ],
    education_evidence$
      level_max[
        observation_index
      ]
  )
}


message(
  "Education consolidation inputs validated successfully."
)


# ---------------------------------------------------------------------
# 4. Define operational population scope
# ---------------------------------------------------------------------
# evidence_fallback_resident is based only on observable register
# evidence. final_estimated_resident is deliberately not used because
# the synthetic clarification simulation depends on hidden residence
# truth.
#
# Age in the population-estimation dataset refers to the end of 2025.
# Therefore age >= 16 corresponds to age >= 15 in reference year 2024.

population_scope <-
  population %>%

  transmute(
    person_id,
    age_2025 = age,
    age_2024 =
      if_else(
        is.na(age),
        NA_real_,
        age - 1
      ),
    evidence_fallback_resident,
    population_scope = case_when(

      evidence_fallback_resident != 1L ~
        "not_fallback_resident",

      is.na(age) ~
        "unknown_age",

      age >= 16L ~
        "eligible_2024_age_15_plus",

      TRUE ~
        "below_age_threshold"
    )
  )


education_population_scope_summary <-
  population_scope %>%

  count(
    population_scope,
    name = "persons"
  ) %>%

  mutate(
    total_observed_persons =
      sum(
        persons
      ),

    share_of_observed_persons =
      persons /
      total_observed_persons
  ) %>%

  arrange(
    factor(
      population_scope,
      levels = c(
        "eligible_2024_age_15_plus",
        "below_age_threshold",
        "unknown_age",
        "not_fallback_resident"
      )
    )
  )


education_target_population <-
  population_scope %>%

  filter(
    population_scope ==
      "eligible_2024_age_15_plus"
  ) %>%

  select(
    person_id,
    age_2024
  )


if (
  nrow(
    education_target_population
  ) == 0L
) {
  stop(
    "Education target population is empty.",
    call. = FALSE
  )
}


if (
  any(
    is.na(
      education_target_population$
        age_2024
    )
  )
) {
  stop(
    "Education target population contains missing age.",
    call. = FALSE
  )
}


if (
  any(
    education_target_population$
      age_2024 < 15L
  )
) {
  stop(
    "Education target population contains persons below age 15 in 2024.",
    call. = FALSE
  )
}


message(
  "Operational education target population defined successfully."
)


# ---------------------------------------------------------------------
# 5. Restrict usable evidence to the operational target population
# ---------------------------------------------------------------------

target_evidence <-
  education_evidence %>%

  inner_join(
    education_target_population,
    by = "person_id"
  )


if (
  any(
    !target_evidence$
      reference_year %in%
      c(
        2022L,
        2024L
      )
  )
) {
  stop(
    "Target education evidence contains an unexpected reference year.",
    call. = FALSE
  )
}


message(
  "Usable target-population education evidence: ",
  nrow(
    target_evidence
  )
)


# ---------------------------------------------------------------------
# 6. Consolidate evidence within each person/reference year
# ---------------------------------------------------------------------
# Multiple source observations in the same reference year are combined
# only when their harmonised attainment ranges overlap.
#
# Compatible ranges are narrowed to their common information.
# Disjoint ranges remain unresolved and are marked for review rather
# than being widened into an artificial compromise range.

consolidate_person_year_group <- function(
  person_year_evidence
) {

  person_year_evidence <-
    person_year_evidence %>%
    arrange(
      source,
      source_row_id
    )


  consolidation <-
    consolidate_same_year_attainment(
      person_year_evidence$level_min,
      person_year_evidence$level_max
    )


  tibble(
    n_observations =
      nrow(
        person_year_evidence
      ),

    n_sources =
      n_distinct(
        person_year_evidence$source
      ),

    evidence_sources =
      paste(
        person_year_evidence$source,
        collapse = "|"
      ),

    evidence_source_rows =
      paste(
        paste0(
          person_year_evidence$source,
          ":",
          person_year_evidence$source_row_id
        ),
        collapse = "|"
      ),

    level_min =
      consolidation$level_min,

    level_max =
      consolidation$level_max,

    same_year_status =
      if (
        consolidation$compatible
      ) {
        "accepted"
      } else {
        "review_required"
      },

    same_year_decision_reason =
      consolidation$decision_reason
  )
}


education_attainment_person_year <-
  target_evidence %>%

  group_by(
    person_id,
    reference_year
  ) %>%

  group_modify(
    ~ consolidate_person_year_group(
      .x
    )
  ) %>%

  ungroup() %>%

  left_join(
    education_target_population,
    by = "person_id"
  ) %>%

  arrange(
    person_id,
    reference_year
  )


# ---------------------------------------------------------------------
# 7. Validate same-year consolidation
# ---------------------------------------------------------------------

if (
  anyDuplicated(
    education_attainment_person_year[
      c(
        "person_id",
        "reference_year"
      )
    ]
  ) > 0L
) {
  stop(
    paste(
      "Person-year education output contains duplicated",
      "person/reference-year keys."
    ),
    call. = FALSE
  )
}


expected_person_year_records <-
  target_evidence %>%

  distinct(
    person_id,
    reference_year
  ) %>%

  nrow()


if (
  nrow(
    education_attainment_person_year
  ) !=
    expected_person_year_records
) {
  stop(
    paste(
      "Consolidated person-year records do not reconcile",
      "to distinct target-evidence person/year keys."
    ),
    call. = FALSE
  )
}


if (
  any(
    (
      education_attainment_person_year$
        same_year_decision_reason ==
        "same_year_conflict"
    ) !=
      (
        education_attainment_person_year$
          same_year_status ==
          "review_required"
      )
  )
) {
  stop(
    paste(
      "Same-year conflict classification is inconsistent",
      "with review-required status."
    ),
    call. = FALSE
  )
}


if (
  any(
    education_attainment_person_year$
      same_year_status ==
      "accepted" &
      (
        is.na(
          education_attainment_person_year$
            level_min
        ) |
          is.na(
            education_attainment_person_year$
              level_max
          )
      )
  )
) {
  stop(
    paste(
      "Accepted person-year education states contain",
      "missing attainment ranges."
    ),
    call. = FALSE
  )
}


if (
  any(
    education_attainment_person_year$
      same_year_status ==
      "review_required" &
      (
        !is.na(
          education_attainment_person_year$
            level_min
        ) |
          !is.na(
            education_attainment_person_year$
              level_max
          )
      )
  )
) {
  stop(
    paste(
      "Conflicting person-year education states unexpectedly",
      "contain consolidated attainment ranges."
    ),
    call. = FALSE
  )
}


if (
  any(
    education_attainment_person_year$
      n_observations <
      1L
  )
) {
  stop(
    "Person-year education output contains zero-observation groups.",
    call. = FALSE
  )
}


message(
  "Same-year education evidence consolidated successfully."
)

message(
  "Consolidated person-year records: ",
  nrow(
    education_attainment_person_year
  )
)

message(
  "Same-year conflicts: ",
  sum(
    education_attainment_person_year$
      same_year_decision_reason ==
      "same_year_conflict"
  )
)


# ---------------------------------------------------------------------
# 8. Prepare reference-year states for longitudinal reconciliation
# ---------------------------------------------------------------------

state_2022 <-
  education_attainment_person_year %>%

  filter(
    reference_year == 2022L
  )


if (
  any(
    state_2022$
      same_year_status !=
      "accepted"
  )
) {
  stop(
    paste(
      "The 2022 baseline contains unresolved same-year evidence",
      "and cannot be used as an accepted prior state."
    ),
    call. = FALSE
  )
}


state_2022 <-
  state_2022 %>%

  transmute(
    person_id,

    n_observations_2022 =
      n_observations,

    n_sources_2022 =
      n_sources,

    evidence_sources_2022 =
      evidence_sources,

    evidence_source_rows_2022 =
      evidence_source_rows,

    level_min_2022 =
      level_min,

    level_max_2022 =
      level_max,

    same_year_status_2022 =
      same_year_status,

    same_year_decision_reason_2022 =
      same_year_decision_reason
  )


state_2024 <-
  education_attainment_person_year %>%

  filter(
    reference_year == 2024L
  ) %>%

  transmute(
    person_id,

    n_observations_2024 =
      n_observations,

    n_sources_2024 =
      n_sources,

    evidence_sources_2024 =
      evidence_sources,

    evidence_source_rows_2024 =
      evidence_source_rows,

    level_min_2024 =
      level_min,

    level_max_2024 =
      level_max,

    same_year_status_2024 =
      same_year_status,

    same_year_decision_reason_2024 =
      same_year_decision_reason
  )


# ---------------------------------------------------------------------
# 9. Build complete target-person reconciliation frame
# ---------------------------------------------------------------------

longitudinal_base <-
  education_target_population %>%

  left_join(
    state_2022,
    by = "person_id"
  ) %>%

  left_join(
    state_2024,
    by = "person_id"
  ) %>%

  mutate(
    has_2022_evidence =
      !is.na(
        same_year_status_2022
      ),

    has_2024_evidence =
      !is.na(
        same_year_status_2024
      ),

    same_year_conflict_2024 =
      coalesce(
        same_year_decision_reason_2024 ==
          "same_year_conflict",
        FALSE
      )
  )


if (
  nrow(
    longitudinal_base
  ) !=
    nrow(
      education_target_population
    )
) {
  stop(
    "Longitudinal joins changed the education target-population size.",
    call. = FALSE
  )
}


if (
  anyDuplicated(
    longitudinal_base$person_id
  ) > 0L
) {
  stop(
    "Longitudinal reconciliation frame contains duplicated persons.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 10. Resolve final 2024 attainment state
# ---------------------------------------------------------------------

attainment_resolutions <-
  lapply(
    seq_len(
      nrow(
        longitudinal_base
      )
    ),
    function(
      row_index
    ) {

      resolve_attainment_state(
        previous_min =
          longitudinal_base$
            level_min_2022[
              row_index
            ],

        previous_max =
          longitudinal_base$
            level_max_2022[
              row_index
            ],

        new_min =
          longitudinal_base$
            level_min_2024[
              row_index
            ],

        new_max =
          longitudinal_base$
            level_max_2024[
              row_index
            ],

        new_same_year_conflict =
          longitudinal_base$
            same_year_conflict_2024[
              row_index
            ]
      )
    }
  )


education_attainment_2024 <-
  longitudinal_base %>%

  mutate(
    attainment_status =
      vapply(
        attainment_resolutions,
        function(
          result
        ) {
          result$status
        },
        character(1)
      ),

    attainment_level_min =
      vapply(
        attainment_resolutions,
        function(
          result
        ) {
          result$level_min
        },
        integer(1)
      ),

    attainment_level_max =
      vapply(
        attainment_resolutions,
        function(
          result
        ) {
          result$level_max
        },
        integer(1)
      ),

    decision_reason =
      vapply(
        attainment_resolutions,
        function(
          result
        ) {
          result$decision_reason
        },
        character(1)
      )
  ) %>%

  arrange(
    person_id
  )


# ---------------------------------------------------------------------
# 11. Validate final longitudinal attainment states
# ---------------------------------------------------------------------

if (
  nrow(
    education_attainment_2024
  ) !=
    nrow(
      education_target_population
    )
) {
  stop(
    paste(
      "Final education-attainment population does not reconcile",
      "to the operational target population."
    ),
    call. = FALSE
  )
}


if (
  anyDuplicated(
    education_attainment_2024$person_id
  ) > 0L
) {
  stop(
    "Final education-attainment output contains duplicated persons.",
    call. = FALSE
  )
}


if (
  any(
    !education_attainment_2024$
      attainment_status %in%
      c(
        "accepted",
        "review_required",
        "not_reported"
      )
  )
) {
  stop(
    "Unexpected final education-attainment status.",
    call. = FALSE
  )
}


if (
  any(
    education_attainment_2024$
      attainment_status ==
      "accepted" &
      (
        is.na(
          education_attainment_2024$
            attainment_level_min
        ) |
          is.na(
            education_attainment_2024$
              attainment_level_max
          )
      )
  )
) {
  stop(
    "Accepted final attainment states contain missing ranges.",
    call. = FALSE
  )
}


if (
  any(
    education_attainment_2024$
      attainment_status ==
      "not_reported" &
      (
        !is.na(
          education_attainment_2024$
            attainment_level_min
        ) |
          !is.na(
            education_attainment_2024$
              attainment_level_max
          )
      )
  )
) {
  stop(
    "Not-reported attainment states unexpectedly contain ranges.",
    call. = FALSE
  )
}


if (
  any(
    education_attainment_2024$
      decision_reason ==
      "same_year_conflict" &
      (
        !is.na(
          education_attainment_2024$
            attainment_level_min
        ) |
          !is.na(
            education_attainment_2024$
              attainment_level_max
          )
      )
  )
) {
  stop(
    paste(
      "Unresolved same-year conflicts unexpectedly contain",
      "final attainment ranges."
    ),
    call. = FALSE
  )
}


if (
  any(
    education_attainment_2024$
      decision_reason %in%
      c(
        "same_year_conflict_retained_prior",
        "temporal_regression"
      ) &
      (
        is.na(
          education_attainment_2024$
            attainment_level_min
        ) |
          is.na(
            education_attainment_2024$
              attainment_level_max
          )
      )
  )
) {
  stop(
    paste(
      "Review-required cases with a retained prior state",
      "contain missing final attainment ranges."
    ),
    call. = FALSE
  )
}


message(
  "Longitudinal education attainment reconciled successfully."
)


# ---------------------------------------------------------------------
# 12. Create reconciliation summary
# ---------------------------------------------------------------------

education_reconciliation_summary <-
  education_attainment_2024 %>%

  count(
    attainment_status,
    decision_reason,
    name = "persons"
  ) %>%

  mutate(
    target_population =
      sum(
        persons
      ),

    share_of_target_population =
      persons /
      target_population
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
    desc(persons),
    decision_reason
  )


# ---------------------------------------------------------------------
# 13. Validate summary reconciliation and truth separation
# ---------------------------------------------------------------------

if (
  sum(
    education_population_scope_summary$
      persons
  ) !=
    nrow(
      population
    )
) {
  stop(
    "Population-scope summary does not reconcile to observed persons.",
    call. = FALSE
  )
}


if (
  sum(
    education_reconciliation_summary$
      persons
  ) !=
    nrow(
      education_target_population
    )
) {
  stop(
    "Education reconciliation summary does not reconcile to target population.",
    call. = FALSE
  )
}


if (
  abs(
    sum(
      education_population_scope_summary$
        share_of_observed_persons
    ) -
      1
  ) >
    1e-12
) {
  stop(
    "Population-scope shares do not sum to one.",
    call. = FALSE
  )
}


if (
  abs(
    sum(
      education_reconciliation_summary$
        share_of_target_population
    ) -
      1
  ) >
    1e-12
) {
  stop(
    "Reconciliation shares do not sum to one.",
    call. = FALSE
  )
}


operational_output_columns <- c(
  names(
    education_attainment_person_year
  ),
  names(
    education_attainment_2024
  ),
  names(
    education_population_scope_summary
  ),
  names(
    education_reconciliation_summary
  )
)


truth_like_output_columns <-
  unique(
    operational_output_columns[
      grepl(
        "^true_|truth",
        operational_output_columns,
        ignore.case = TRUE
      )
    ]
  )


if (
  length(
    truth_like_output_columns
  ) > 0L
) {
  stop(
    paste(
      "Hidden-truth-like columns detected in operational education outputs:",
      paste(
        truth_like_output_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 14. Write consolidated education outputs
# ---------------------------------------------------------------------

write_csv(
  education_attainment_person_year,
  file.path(
    education_processed_dir,
    "education_attainment_person_year.csv"
  ),
  na = ""
)


write_csv(
  education_attainment_2024,
  file.path(
    education_processed_dir,
    "education_attainment_2024.csv"
  ),
  na = ""
)


write_csv(
  education_population_scope_summary,
  file.path(
    education_output_tables_dir,
    "education_population_scope_summary.csv"
  )
)


write_csv(
  education_reconciliation_summary,
  file.path(
    education_output_tables_dir,
    "education_reconciliation_summary.csv"
  )
)


message(
  "Education consolidation outputs written successfully."
)

message(
  "2024 education target population: ",
  nrow(
    education_attainment_2024
  )
)

message(
  "Accepted attainment states: ",
  sum(
    education_attainment_2024$
      attainment_status ==
      "accepted"
  )
)

message(
  "Review-required attainment states: ",
  sum(
    education_attainment_2024$
      attainment_status ==
      "review_required"
  )
)

message(
  "Not-reported attainment states: ",
  sum(
    education_attainment_2024$
      attainment_status ==
      "not_reported"
  )
)
