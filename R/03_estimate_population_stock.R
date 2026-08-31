# =====================================================================
# 03_estimate_population_stock.R
# Population Stock Estimation and Synthetic Clarification
# Version 2
# ---------------------------------------------------------------------
# This script converts the integrated person/address evidence created in
# 02_integrate_activity_signals.R into population-stock estimates.
#
# It performs:
#   - construction of a population-register baseline
#   - construction of an evidence-based fallback estimate
#   - evidence-based selection of cases for synthetic clarification
#   - simulation of an imperfect clarification outcome
#   - construction of a final clarification-assisted population estimate
#   - evaluation against the hidden synthetic ground truth
#   - overall, regional, age-group, and quality summaries
#
# Important methodological distinction:
#
#   Clarification TARGETING uses only observable register evidence.
#
#   Hidden synthetic truth is accessed only after targeting has been
#   completed. It is then used:
#
#     1. to generate an imperfect synthetic clarification outcome, and
#     2. to evaluate the resulting estimates.
#
#   The clarification mechanism is an illustrative simulation component.
#   Its probabilities are not empirical estimates for Germany and do not
#   reproduce an official Destatis procedure.
#
# Outputs:
#   data/processed/person_population_estimate.csv
#   output/tables/population_estimation_overall.csv
#   output/tables/population_estimation_by_region.csv
#   output/tables/population_estimation_by_age_group.csv
#   output/tables/estimation_quality_summary.csv
#   output/tables/clarification_summary.csv
# =====================================================================


# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------

library(dplyr)
library(readr)


# ---------------------------------------------------------------------
# 1. Ensure output directories exist
# ---------------------------------------------------------------------

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "output/tables",
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------------------
# 2. Load integrated observable data
# ---------------------------------------------------------------------

person_register <- read_csv(
  "data/processed/person_register_integrated.csv",
  show_col_types = FALSE
)

message(
  "Integrated person evidence loaded: ",
  nrow(person_register),
  " rows."
)


# ---------------------------------------------------------------------
# 3. Structural checks before estimation
# ---------------------------------------------------------------------

if (
  nrow(person_register) !=
    n_distinct(person_register$person_id)
) {

  stop(
    "Critical estimation error: duplicate person IDs in integrated person file.",
    call. = FALSE
  )
}


forbidden_truth_columns <- c(
  "resident_true",
  "true_resident",
  "coverage_status_true",
  "true_address_id",
  "former_address_id",
  "true_region_code",
  "true_municipality_code",
  "undercoverage_flag_true",
  "overcoverage_flag_true"
)


truth_columns_found <- intersect(
  forbidden_truth_columns,
  names(person_register)
)


if (
  length(truth_columns_found) > 0
) {

  stop(
    paste0(
      "Critical estimation error: hidden truth found in operational input: ",
      paste(
        truth_columns_found,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 4. Define register baseline, evidence fallback, and clarification targets
# ---------------------------------------------------------------------

message("Constructing observable-data estimation rules...")


estimation_base <- person_register %>%

  mutate(
    # ---------------------------------------------------------------
    # Population-register baseline
    # ---------------------------------------------------------------

    register_baseline_resident =
      as.integer(
        in_population_register == 1L
      ),

    # ---------------------------------------------------------------
    # Evidence-based fallback
    # ---------------------------------------------------------------
    # Registration remains the primary evidence for persons already in
    # the population register.
    #
    # An auxiliary-only person is included by the fallback rule only
    # where there is:
    #   - at least one positive activity signal, and
    #   - one consistent auxiliary address.
    #
    # This fallback is deliberately conservative and is also used when
    # a targeted clarification case does not produce a response.

    evidence_fallback_resident = case_when(

      in_population_register == 1L ~
        1L,

      in_population_register == 0L &
        n_activity_signals > 0L &
        !is.na(
          consistent_auxiliary_address_id
        ) ~
        1L,

      TRUE ~
        0L
    ),

    # ---------------------------------------------------------------
    # Residence-status clarification targeting
    # ---------------------------------------------------------------
    # Address inconsistencies identified in Script 03 remain useful
    # data-quality and address-clarification indicators, but they are
    # not by themselves treated as evidence of non-residence here.
    #
    # Residence-status clarification is targeted at:
    #
    #   1. all auxiliary-only persons, because population-register
    #      absence combined with auxiliary-source presence requires
    #      resolution; and
    #
    #   2. registered persons aged 18-64 with no positive auxiliary
    #      activity signal.
    #
    # Registered persons with unknown age and no positive activity are
    # also retained as unresolved residence-status cases.
    #
    # Absence of activity is therefore used as a reason for additional
    # checking, not as direct evidence of non-residence.

    residence_clarification_target_reason = case_when(

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
        n_activity_signals == 0L &
        age_group %in%
          c(
            "18-24",
            "25-39",
            "40-64"
          ) ~
        "registered_age_18_64_without_positive_activity",

      in_population_register == 1L &
        n_activity_signals == 0L &
        is.na(age_group) ~
        "registered_unknown_age_without_positive_activity",

      TRUE ~
        "none"
    ),

    flag_residence_clarification_target =
      residence_clarification_target_reason != "none"
  )

# ---------------------------------------------------------------------
# 5. Validate clarification targeting before accessing hidden truth
# ---------------------------------------------------------------------

message(
  "Cases targeted for synthetic residence clarification = ",
  sum(
    estimation_base$flag_residence_clarification_target,
    na.rm = TRUE
  )
)


message("Residence-clarification target distribution:")

print(
  estimation_base %>%

    count(
      residence_clarification_target_reason,
      sort = TRUE
    )
)


# ---------------------------------------------------------------------
# 6. Load hidden synthetic truth
# ---------------------------------------------------------------------
# Truth is loaded only after all clarification-target decisions have
# already been made from observable data.

truth_raw <- read_csv(
  "data/raw/synthetic_population_truth.csv",
  show_col_types = FALSE
)


if (
  nrow(truth_raw) !=
    n_distinct(truth_raw$person_id)
) {

  stop(
    "Critical estimation error: duplicate person IDs in synthetic truth.",
    call. = FALSE
  )
}


unknown_observed_ids <- setdiff(
  estimation_base$person_id,
  truth_raw$person_id
)


if (
  length(unknown_observed_ids) > 0
) {

  stop(
    paste0(
      "Critical estimation error: ",
      length(unknown_observed_ids),
      " observed person IDs are absent from the synthetic truth universe."
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 7. Simulate imperfect clarification outcomes
# ---------------------------------------------------------------------
# The following parameters are illustrative simulation assumptions.
#
# A targeted person:
#   - responds with probability 0.90
#   - if a response is obtained, the synthetic clarification result
#     reflects the latent residence status correctly with probability
#     0.98
#
# Non-response falls back to the evidence-based rule defined above.

clarification_response_probability <- 0.90
clarification_result_accuracy <- 0.98

set.seed(2031)


clarification_simulation <- estimation_base %>%

  left_join(
    truth_raw %>%
      select(
        person_id,
        true_resident
      ),
    by = "person_id"
  ) %>%

  mutate(
    .clarification_response_draw =
      runif(n()),

    .clarification_accuracy_draw =
      runif(n()),

    clarification_response_flag = case_when(

      flag_residence_clarification_target ~
        as.integer(
          .clarification_response_draw <
            clarification_response_probability
        ),

      TRUE ~
        NA_integer_
    ),

    clarification_estimated_resident = case_when(

      clarification_response_flag == 1L &
        .clarification_accuracy_draw <
          clarification_result_accuracy ~
        as.integer(
          true_resident
        ),

      clarification_response_flag == 1L ~
        1L -
        as.integer(
          true_resident
        ),

      TRUE ~
        NA_integer_
    ),

    clarification_outcome = case_when(

      !flag_residence_clarification_target ~
        "not_targeted",

      clarification_response_flag == 0L ~
        "unresolved_nonresponse",

      clarification_estimated_resident == 1L ~
        "resident",

      clarification_estimated_resident == 0L ~
        "not_resident",

      TRUE ~
        "unresolved"
    ),

    final_estimated_resident =
      coalesce(
        clarification_estimated_resident,
        evidence_fallback_resident
      )
  )


# ---------------------------------------------------------------------
# 8. Create operational person-level estimation output
# ---------------------------------------------------------------------
# Hidden truth and simulation-only random draws are removed before the
# person-level estimation dataset is written.

person_population_estimate <- clarification_simulation %>%

  select(
    -true_resident,
    -.clarification_response_draw,
    -.clarification_accuracy_draw
  ) %>%

  arrange(
    person_id
  )


output_truth_columns <- intersect(
  forbidden_truth_columns,
  names(person_population_estimate)
)


if (
  length(output_truth_columns) > 0
) {

  stop(
    paste0(
      "Critical estimation error: hidden truth leaked into person-level output: ",
      paste(
        output_truth_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


if (
  any(
    !person_population_estimate$
      final_estimated_resident %in%
      c(
        0L,
        1L
      )
  )
) {

  stop(
    "Critical estimation error: invalid final_estimated_resident values.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 9. Overall population-stock estimates
# ---------------------------------------------------------------------

message("Computing overall population estimates...")


true_population_count <- sum(
  truth_raw$true_resident == 1L,
  na.rm = TRUE
)


synthetic_universe_count <- nrow(
  truth_raw
)


observed_person_count <- nrow(
  person_population_estimate
)


unobserved_person_count <- sum(
  !truth_raw$person_id %in%
    person_population_estimate$person_id
)


unobserved_true_resident_count <- sum(
  truth_raw$true_resident == 1L &
    !truth_raw$person_id %in%
      person_population_estimate$person_id,
  na.rm = TRUE
)


register_baseline_estimate <- sum(
  person_population_estimate$
    register_baseline_resident,
  na.rm = TRUE
)


evidence_fallback_estimate <- sum(
  person_population_estimate$
    evidence_fallback_resident,
  na.rm = TRUE
)


clarification_assisted_estimate <- sum(
  person_population_estimate$
    final_estimated_resident,
  na.rm = TRUE
)


population_estimation_overall <- tibble(
  synthetic_universe_count =
    synthetic_universe_count,

  true_population_count =
    true_population_count,

  observed_person_count =
    observed_person_count,

  population_register_count =
    sum(
      person_population_estimate$
        in_population_register == 1L,
      na.rm = TRUE
    ),

  auxiliary_only_observed_count =
    sum(
      person_population_estimate$
        flag_auxiliary_only,
      na.rm = TRUE
    ),

  unobserved_person_count =
    unobserved_person_count,

  unobserved_true_resident_count =
    unobserved_true_resident_count,

  clarification_target_count =
    sum(
      person_population_estimate$
        flag_residence_clarification_target,
      na.rm = TRUE
    ),

  clarification_response_count =
    sum(
      person_population_estimate$
        clarification_response_flag == 1L,
      na.rm = TRUE
    ),

  clarification_nonresponse_count =
    sum(
      person_population_estimate$
        clarification_response_flag == 0L,
      na.rm = TRUE
    ),

  clarification_response_probability_parameter =
    clarification_response_probability,

  clarification_result_accuracy_parameter =
    clarification_result_accuracy,

  register_baseline_estimate =
    register_baseline_estimate,

  register_baseline_error_count =
    register_baseline_estimate -
    true_population_count,

  register_baseline_error_rate =
    (
      register_baseline_estimate -
        true_population_count
    ) /
    true_population_count,

  evidence_fallback_estimate =
    evidence_fallback_estimate,

  evidence_fallback_error_count =
    evidence_fallback_estimate -
    true_population_count,

  evidence_fallback_error_rate =
    (
      evidence_fallback_estimate -
        true_population_count
    ) /
    true_population_count,

  clarification_assisted_estimate =
    clarification_assisted_estimate,

  clarification_assisted_error_count =
    clarification_assisted_estimate -
    true_population_count,

  clarification_assisted_error_rate =
    (
      clarification_assisted_estimate -
        true_population_count
    ) /
    true_population_count
)


# ---------------------------------------------------------------------
# 10. Clarification-process summary
# ---------------------------------------------------------------------

message("Computing clarification summary...")


clarification_summary <- person_population_estimate %>%

  filter(
    flag_residence_clarification_target
  ) %>%

  group_by(
    residence_clarification_target_reason
  ) %>%

  summarise(
    target_cases =
      n(),

    responses =
      sum(
        clarification_response_flag == 1L,
        na.rm = TRUE
      ),

    nonresponses =
      sum(
        clarification_response_flag == 0L,
        na.rm = TRUE
      ),

    response_rate =
      mean(
        clarification_response_flag == 1L,
        na.rm = TRUE
      ),

    clarification_resident_results =
      sum(
        clarification_estimated_resident == 1L,
        na.rm = TRUE
      ),

    clarification_nonresident_results =
      sum(
        clarification_estimated_resident == 0L,
        na.rm = TRUE
      ),

    final_estimated_residents =
      sum(
        final_estimated_resident == 1L,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) %>%

  arrange(
    desc(target_cases),
    residence_clarification_target_reason
  )


# ---------------------------------------------------------------------
# 11. Estimation-quality evaluation over full synthetic universe
# ---------------------------------------------------------------------
# Persons absent from every observable register receive a prediction of
# non-residence because the operational system has no person-level
# evidence with which to identify them.
#
# This means the evaluation explicitly captures residual undercoverage
# that cannot be resolved by record-level classification alone.

message("Computing estimation-quality summary...")


evaluation_universe <- truth_raw %>%

  select(
    person_id,
    true_resident
  ) %>%

  left_join(
    person_population_estimate %>%
      select(
        person_id,
        register_baseline_resident,
        evidence_fallback_resident,
        final_estimated_resident
      ),
    by = "person_id"
  ) %>%

  mutate(
    register_baseline_resident =
      coalesce(
        register_baseline_resident,
        0L
      ),

    evidence_fallback_resident =
      coalesce(
        evidence_fallback_resident,
        0L
      ),

    final_estimated_resident =
      coalesce(
        final_estimated_resident,
        0L
      )
  )


safe_ratio <- function(
    numerator,
    denominator
) {

  if (
    denominator == 0
  ) {

    NA_real_

  } else {

    numerator /
      denominator
  }
}


calculate_quality <- function(
    actual,
    predicted,
    method_name
) {

  true_positive <- sum(
    predicted == 1L &
      actual == 1L,
    na.rm = TRUE
  )

  false_positive <- sum(
    predicted == 1L &
      actual == 0L,
    na.rm = TRUE
  )

  true_negative <- sum(
    predicted == 0L &
      actual == 0L,
    na.rm = TRUE
  )

  false_negative <- sum(
    predicted == 0L &
      actual == 1L,
    na.rm = TRUE
  )

  total <- (
    true_positive +
      false_positive +
      true_negative +
      false_negative
  )


  tibble(
    estimation_method =
      method_name,

    evaluated_persons =
      total,

    true_positive =
      true_positive,

    false_positive =
      false_positive,

    true_negative =
      true_negative,

    false_negative =
      false_negative,

    accuracy =
      safe_ratio(
        true_positive +
          true_negative,
        total
      ),

    precision =
      safe_ratio(
        true_positive,
        true_positive +
          false_positive
      ),

    recall =
      safe_ratio(
        true_positive,
        true_positive +
          false_negative
      ),

    specificity =
      safe_ratio(
        true_negative,
        true_negative +
          false_positive
      ),

    false_positive_rate =
      safe_ratio(
        false_positive,
        false_positive +
          true_negative
      ),

    false_negative_rate =
      safe_ratio(
        false_negative,
        false_negative +
          true_positive
      )
  )
}


estimation_quality_summary <- bind_rows(

  calculate_quality(
    actual =
      evaluation_universe$
        true_resident,

    predicted =
      evaluation_universe$
        register_baseline_resident,

    method_name =
      "population_register_baseline"
  ),

  calculate_quality(
    actual =
      evaluation_universe$
        true_resident,

    predicted =
      evaluation_universe$
        evidence_fallback_resident,

    method_name =
      "evidence_fallback_without_clarification"
  ),

  calculate_quality(
    actual =
      evaluation_universe$
        true_resident,

    predicted =
      evaluation_universe$
        final_estimated_resident,

    method_name =
      "clarification_assisted"
  )
)


# ---------------------------------------------------------------------
# 12. Region-level population estimates
# ---------------------------------------------------------------------

message("Computing region-level estimates...")


baseline_by_region <- person_population_estimate %>%

  filter(
    register_baseline_resident == 1L
  ) %>%

  mutate(
    summary_region_code =
      coalesce(
        analysis_region_code,
        "UNRESOLVED"
      )
  ) %>%

  count(
    summary_region_code,
    name = "register_baseline_count"
  )


fallback_by_region <- person_population_estimate %>%

  filter(
    evidence_fallback_resident == 1L
  ) %>%

  mutate(
    summary_region_code =
      coalesce(
        analysis_region_code,
        "UNRESOLVED"
      )
  ) %>%

  count(
    summary_region_code,
    name = "evidence_fallback_count"
  )


final_by_region <- person_population_estimate %>%

  filter(
    final_estimated_resident == 1L
  ) %>%

  mutate(
    summary_region_code =
      coalesce(
        analysis_region_code,
        "UNRESOLVED"
      )
  ) %>%

  count(
    summary_region_code,
    name = "clarification_assisted_count"
  )


truth_by_region <- truth_raw %>%

  filter(
    true_resident == 1L
  ) %>%

  count(
    true_region_code,
    name = "true_resident_count"
  ) %>%

  rename(
    summary_region_code =
      true_region_code
  )


population_estimation_by_region <- baseline_by_region %>%

  full_join(
    fallback_by_region,
    by = "summary_region_code"
  ) %>%

  full_join(
    final_by_region,
    by = "summary_region_code"
  ) %>%

  full_join(
    truth_by_region,
    by = "summary_region_code"
  ) %>%

  mutate(
    register_baseline_count =
      coalesce(
        register_baseline_count,
        0L
      ),

    evidence_fallback_count =
      coalesce(
        evidence_fallback_count,
        0L
      ),

    clarification_assisted_count =
      coalesce(
        clarification_assisted_count,
        0L
      ),

    true_resident_count =
      coalesce(
        true_resident_count,
        0L
      ),

    register_baseline_error =
      register_baseline_count -
      true_resident_count,

    evidence_fallback_error =
      evidence_fallback_count -
      true_resident_count,

    clarification_assisted_error =
      clarification_assisted_count -
      true_resident_count,

    register_baseline_error_rate =
      if_else(
        true_resident_count > 0L,
        register_baseline_error /
          true_resident_count,
        NA_real_
      ),

    evidence_fallback_error_rate =
      if_else(
        true_resident_count > 0L,
        evidence_fallback_error /
          true_resident_count,
        NA_real_
      ),

    clarification_assisted_error_rate =
      if_else(
        true_resident_count > 0L,
        clarification_assisted_error /
          true_resident_count,
        NA_real_
      )
  ) %>%

  arrange(
    summary_region_code ==
      "UNRESOLVED",
    summary_region_code
  )


# ---------------------------------------------------------------------
# 13. Age-group population estimates
# ---------------------------------------------------------------------
# Auxiliary-only persons generally lack population-register age
# information. They therefore remain in an explicit UNKNOWN group
# rather than being assigned hidden true ages.

message("Computing age-group estimates...")


baseline_by_age <- person_population_estimate %>%

  filter(
    register_baseline_resident == 1L
  ) %>%

  mutate(
    analysis_age_group =
      coalesce(
        age_group,
        "UNKNOWN"
      )
  ) %>%

  count(
    analysis_age_group,
    name = "register_baseline_count"
  )


fallback_by_age <- person_population_estimate %>%

  filter(
    evidence_fallback_resident == 1L
  ) %>%

  mutate(
    analysis_age_group =
      coalesce(
        age_group,
        "UNKNOWN"
      )
  ) %>%

  count(
    analysis_age_group,
    name = "evidence_fallback_count"
  )


final_by_age <- person_population_estimate %>%

  filter(
    final_estimated_resident == 1L
  ) %>%

  mutate(
    analysis_age_group =
      coalesce(
        age_group,
        "UNKNOWN"
      )
  ) %>%

  count(
    analysis_age_group,
    name = "clarification_assisted_count"
  )


truth_by_age <- truth_raw %>%

  filter(
    true_resident == 1L
  ) %>%

  count(
    age_group,
    name = "true_resident_count"
  ) %>%

  rename(
    analysis_age_group =
      age_group
  )


population_estimation_by_age_group <- baseline_by_age %>%

  full_join(
    fallback_by_age,
    by = "analysis_age_group"
  ) %>%

  full_join(
    final_by_age,
    by = "analysis_age_group"
  ) %>%

  full_join(
    truth_by_age,
    by = "analysis_age_group"
  ) %>%

  mutate(
    register_baseline_count =
      coalesce(
        register_baseline_count,
        0L
      ),

    evidence_fallback_count =
      coalesce(
        evidence_fallback_count,
        0L
      ),

    clarification_assisted_count =
      coalesce(
        clarification_assisted_count,
        0L
      ),

    true_resident_count =
      coalesce(
        true_resident_count,
        0L
      ),

    register_baseline_error =
      register_baseline_count -
      true_resident_count,

    evidence_fallback_error =
      evidence_fallback_count -
      true_resident_count,

    clarification_assisted_error =
      clarification_assisted_count -
      true_resident_count
  ) %>%

  arrange(
    match(
      analysis_age_group,
      c(
        "0-5",
        "6-17",
        "18-24",
        "25-39",
        "40-64",
        "65-79",
        "80+",
        "UNKNOWN"
      )
    )
  )


# ---------------------------------------------------------------------
# 14. Print diagnostic summaries
# ---------------------------------------------------------------------

message("Overall population-estimation summary:")

print(
  population_estimation_overall
)


message("Estimation-quality summary:")

print(
  estimation_quality_summary
)


message("Clarification summary:")

print(
  clarification_summary
)


# ---------------------------------------------------------------------
# 15. Write outputs
# ---------------------------------------------------------------------

write_csv(
  person_population_estimate,
  "data/processed/person_population_estimate.csv"
)


write_csv(
  population_estimation_overall,
  "output/tables/population_estimation_overall.csv"
)


write_csv(
  population_estimation_by_region,
  "output/tables/population_estimation_by_region.csv"
)


write_csv(
  population_estimation_by_age_group,
  "output/tables/population_estimation_by_age_group.csv"
)


write_csv(
  estimation_quality_summary,
  "output/tables/estimation_quality_summary.csv"
)


write_csv(
  clarification_summary,
  "output/tables/clarification_summary.csv"
)


message(
  "Population estimation completed successfully."
)

message(
  "Processed person estimate written to data/processed/."
)

message(
  "Estimation tables written to output/tables/."
)