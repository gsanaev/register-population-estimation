# =====================================================================
# 04_visualize_results.R
# Visualization of Register-Based Population Estimation Results
# Version 2
# ---------------------------------------------------------------------
# This script creates the core figures for the synthetic register-based
# population-estimation workflow.
#
# It visualizes:
#   - distribution of positive activity signals
#   - distribution of address-evidence situations
#   - overall population-stock estimates across estimation approaches
#   - estimation error by synthetic region
#   - classification quality by estimation approach
#   - residence-clarification target groups
#
# Hidden synthetic truth is used here only for evaluation and reporting.
# It is not incorporated into the operational person-level datasets.
#
# Outputs:
#   output/figures/activity_signal_distribution.png
#   output/figures/address_evidence_distribution.png
#   output/figures/population_estimates_overall.png
#   output/figures/estimation_error_by_region.png
#   output/figures/estimation_quality_by_method.png
#   output/figures/residence_clarification_targets.png
# =====================================================================


# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------

dir.create(
  "output/figures",
  showWarnings = FALSE,
  recursive = TRUE
)


# ---------------------------------------------------------------------
# 2. Load processed data and estimation outputs
# ---------------------------------------------------------------------

person_register <- read_csv(
  "data/processed/person_register_integrated.csv",
  show_col_types = FALSE
)

population_overall <- read_csv(
  "output/tables/population_estimation_overall.csv",
  show_col_types = FALSE
)

population_by_region <- read_csv(
  "output/tables/population_estimation_by_region.csv",
  show_col_types = FALSE
)

quality_summary <- read_csv(
  "output/tables/estimation_quality_summary.csv",
  show_col_types = FALSE
)

clarification_summary <- read_csv(
  "output/tables/clarification_summary.csv",
  show_col_types = FALSE
)


# ---------------------------------------------------------------------
# 3. Basic structural checks
# ---------------------------------------------------------------------

message("Validating visualization inputs...")


if (
  nrow(person_register) !=
    n_distinct(person_register$person_id)
) {

  stop(
    "Critical visualization error: duplicate person IDs in integrated person file.",
    call. = FALSE
  )
}


required_person_columns <- c(
  "person_id",
  "n_activity_signals",
  "address_evidence_status"
)


missing_person_columns <- setdiff(
  required_person_columns,
  names(person_register)
)


if (
  length(missing_person_columns) > 0
) {

  stop(
    paste0(
      "Critical visualization error: missing person-level columns: ",
      paste(
        missing_person_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 4. Common label mappings
# ---------------------------------------------------------------------

method_labels <- c(
  "population_register_baseline" =
    "Population-register baseline",

  "evidence_fallback_without_clarification" =
    "Evidence fallback",

  "clarification_assisted" =
    "Clarification-assisted"
)


address_status_labels <- c(
  "registered_address_supported" =
    "Registered: auxiliary address supports register",

  "registered_no_auxiliary_address" =
    "Registered: no auxiliary address",

  "registered_mixed_auxiliary_addresses" =
    "Registered: mixed auxiliary addresses",

  "registered_auxiliary_addresses_differ" =
    "Registered: auxiliary address differs",

  "auxiliary_only_consistent_address" =
    "Auxiliary-only: consistent address",

  "auxiliary_only_conflicting_addresses" =
    "Auxiliary-only: conflicting addresses",

  "auxiliary_only_no_address" =
    "Auxiliary-only: no usable address",

  "address_evidence_unresolved" =
    "Address evidence unresolved"
)


clarification_reason_labels <- c(
  "registered_age_18_64_without_positive_activity" =
    "Registered age 18-64,\nno positive activity",

  "registered_unknown_age_without_positive_activity" =
    "Registered unknown age,\nno positive activity",

  "auxiliary_only_consistent_address" =
    "Auxiliary-only,\nconsistent address",

  "auxiliary_only_without_consistent_address" =
    "Auxiliary-only,\nno consistent address",

  "auxiliary_only_conflicting_addresses" =
    "Auxiliary-only,\nconflicting addresses"
)


# ---------------------------------------------------------------------
# 5. Figure 1: Distribution of positive activity signals
# ---------------------------------------------------------------------

message("Creating activity-signal distribution...")


activity_signal_plot_data <- person_register %>%

  count(
    n_activity_signals,
    name = "persons"
  ) %>%

  complete(
    n_activity_signals = 0:3,
    fill = list(
      persons = 0L
    )
  ) %>%

  arrange(
    n_activity_signals
  )


p_activity_signals <- activity_signal_plot_data %>%

  ggplot(
    aes(
      x = factor(
        n_activity_signals,
        levels = 0:3
      ),
      y = persons
    )
  ) +

  geom_col() +

  geom_text(
    aes(
      label = format(
        persons,
        big.mark = ",",
        scientific = FALSE
      )
    ),
    vjust = -0.4,
    size = 3.5
  ) +

  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(
      mult = c(
        0,
        0.10
      )
    )
  ) +

  labs(
    title = "Positive Activity Signals Across Observed Persons",
    subtitle = "Employment, tax and education sources",
    x = "Number of positive activity signals",
    y = "Observed persons"
  ) +

  theme_minimal(
    base_size = 11
  )


ggsave(
  "output/figures/activity_signal_distribution.png",
  p_activity_signals,
  width = 7.5,
  height = 5,
  dpi = 300
)


# ---------------------------------------------------------------------
# 6. Figure 2: Address-evidence distribution
# ---------------------------------------------------------------------

message("Creating address-evidence distribution...")


address_evidence_plot_data <- person_register %>%

  count(
    address_evidence_status,
    name = "persons"
  ) %>%

  mutate(
    address_evidence_label =
      recode(
        address_evidence_status,
        !!!address_status_labels,
        .default =
          address_evidence_status
      )
  ) %>%

  arrange(
    persons
  ) %>%

  mutate(
    address_evidence_label =
      factor(
        address_evidence_label,
        levels =
          address_evidence_label
      )
  )


p_address_evidence <- address_evidence_plot_data %>%

  ggplot(
    aes(
      x = address_evidence_label,
      y = persons
    )
  ) +

  geom_col() +

  coord_flip() +

  geom_text(
    aes(
      label = format(
        persons,
        big.mark = ",",
        scientific = FALSE
      )
    ),
    hjust = -0.15,
    size = 3.3
  ) +

  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +

  labs(
    title = "Address Evidence Across Administrative Sources",
    subtitle = "Observed address agreement and inconsistency before residence estimation",
    x = NULL,
    y = "Observed persons"
  ) +

  theme_minimal(
    base_size = 11
  )


ggsave(
  "output/figures/address_evidence_distribution.png",
  p_address_evidence,
  width = 9,
  height = 5.7,
  dpi = 300
)


# ---------------------------------------------------------------------
# 7. Figure 3: Overall population-stock estimates
# ---------------------------------------------------------------------

message("Creating overall population-estimate comparison...")


population_overall_plot_data <- population_overall %>%

  transmute(
    `True synthetic population` =
      true_population_count,

    `Population-register baseline` =
      register_baseline_estimate,

    `Evidence fallback` =
      evidence_fallback_estimate,

    `Clarification-assisted` =
      clarification_assisted_estimate
  ) %>%

  pivot_longer(
    cols = everything(),
    names_to = "estimate_type",
    values_to = "population_count"
  ) %>%

  mutate(
    estimate_type =
      factor(
        estimate_type,
        levels = c(
          "True synthetic population",
          "Population-register baseline",
          "Evidence fallback",
          "Clarification-assisted"
        )
      )
  )


p_population_overall <- population_overall_plot_data %>%

  ggplot(
    aes(
      x = estimate_type,
      y = population_count
    )
  ) +

  geom_col() +

  geom_text(
    aes(
      label = format(
        population_count,
        big.mark = ",",
        scientific = FALSE
      )
    ),
    vjust = -0.4,
    size = 3.5
  ) +

  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(
      mult = c(
        0,
        0.08
      )
    )
  ) +

  labs(
    title = "Population Stock Estimates",
    subtitle = "Comparison with the hidden synthetic benchmark",
    x = NULL,
    y = "Population count"
  ) +

  theme_minimal(
    base_size = 11
  ) +

  theme(
    axis.text.x =
      element_text(
        angle = 18,
        hjust = 1
      )
  )


ggsave(
  "output/figures/population_estimates_overall.png",
  p_population_overall,
  width = 8,
  height = 5.4,
  dpi = 300
)


# ---------------------------------------------------------------------
# 8. Figure 4: Estimation error by synthetic region
# ---------------------------------------------------------------------

message("Creating regional estimation-error comparison...")


regional_error_plot_data <- population_by_region %>%

  filter(
    summary_region_code !=
      "UNRESOLVED"
  ) %>%

  select(
    summary_region_code,
    register_baseline_error_rate,
    evidence_fallback_error_rate,
    clarification_assisted_error_rate
  ) %>%

  pivot_longer(
    cols = c(
      register_baseline_error_rate,
      evidence_fallback_error_rate,
      clarification_assisted_error_rate
    ),
    names_to = "estimation_method",
    values_to = "error_rate"
  ) %>%

  mutate(
    estimation_method = recode(
      estimation_method,

      "register_baseline_error_rate" =
        "Population-register baseline",

      "evidence_fallback_error_rate" =
        "Evidence fallback",

      "clarification_assisted_error_rate" =
        "Clarification-assisted"
    ),

    estimation_method =
      factor(
        estimation_method,
        levels = c(
          "Population-register baseline",
          "Evidence fallback",
          "Clarification-assisted"
        )
      )
  )


p_regional_error <- regional_error_plot_data %>%

  ggplot(
    aes(
      x = summary_region_code,
      y = error_rate,
      fill = estimation_method
    )
  ) +

  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +

  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.72
  ) +

  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 0.1
    )
  ) +

  labs(
    title = "Population Estimation Error by Synthetic Region",
    subtitle = "Positive values indicate overestimation; negative values indicate underestimation",
    x = "Synthetic region",
    y = "Error relative to hidden synthetic population",
    fill = "Estimation approach"
  ) +

  theme_minimal(
    base_size = 11
  ) +

  theme(
    legend.position = "bottom"
  )


ggsave(
  "output/figures/estimation_error_by_region.png",
  p_regional_error,
  width = 10,
  height = 5.8,
  dpi = 300
)


# ---------------------------------------------------------------------
# 9. Figure 5: Classification quality by estimation method
# ---------------------------------------------------------------------

message("Creating estimation-quality comparison...")


quality_plot_data <- quality_summary %>%

  select(
    estimation_method,
    accuracy,
    precision,
    recall,
    specificity
  ) %>%

  pivot_longer(
    cols = c(
      accuracy,
      precision,
      recall,
      specificity
    ),
    names_to = "quality_metric",
    values_to = "metric_value"
  ) %>%

  mutate(
    estimation_method =
      recode(
        estimation_method,
        !!!method_labels,
        .default =
          estimation_method
      ),

    estimation_method =
      factor(
        estimation_method,
        levels = c(
          "Population-register baseline",
          "Evidence fallback",
          "Clarification-assisted"
        )
      ),

    quality_metric =
      recode(
        quality_metric,
        "accuracy" = "Accuracy",
        "precision" = "Precision",
        "recall" = "Recall",
        "specificity" = "Specificity"
      ),

    quality_metric =
      factor(
        quality_metric,
        levels = c(
          "Accuracy",
          "Precision",
          "Recall",
          "Specificity"
        )
      )
  )


p_quality <- quality_plot_data %>%

  ggplot(
    aes(
      x = quality_metric,
      y = metric_value,
      fill = estimation_method
    )
  ) +

  geom_col(
    position = position_dodge(
      width = 0.8
    ),
    width = 0.72
  ) +

  scale_y_continuous(
    limits = c(
      0,
      1
    ),
    labels = scales::percent_format(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +

  labs(
    title = "Person-Level Classification Quality",
    subtitle = "Evaluation against the full hidden synthetic universe",
    x = NULL,
    y = "Metric value",
    fill = "Estimation approach"
  ) +

  theme_minimal(
    base_size = 11
  ) +

  theme(
    legend.position = "bottom"
  )


ggsave(
  "output/figures/estimation_quality_by_method.png",
  p_quality,
  width = 9,
  height = 5.5,
  dpi = 300
)


# ---------------------------------------------------------------------
# 10. Figure 6: Residence-clarification targets
# ---------------------------------------------------------------------

message("Creating residence-clarification target distribution...")


clarification_plot_data <- clarification_summary %>%

  mutate(
    clarification_label =
      recode(
        residence_clarification_target_reason,
        !!!clarification_reason_labels,
        .default =
          residence_clarification_target_reason
      )
  ) %>%

  arrange(
    target_cases
  ) %>%

  mutate(
    clarification_label =
      factor(
        clarification_label,
        levels =
          clarification_label
      )
  )


p_clarification <- clarification_plot_data %>%

  ggplot(
    aes(
      x = clarification_label,
      y = target_cases
    )
  ) +

  geom_col() +

  coord_flip() +

  geom_text(
    aes(
      label = format(
        target_cases,
        big.mark = ",",
        scientific = FALSE
      )
    ),
    hjust = -0.15,
    size = 3.4
  ) +

  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +

  labs(
    title = "Synthetic Residence-Clarification Targets",
    subtitle = "Targeting is based on observable administrative evidence",
    x = NULL,
    y = "Target cases"
  ) +

  theme_minimal(
    base_size = 11
  )


ggsave(
  "output/figures/residence_clarification_targets.png",
  p_clarification,
  width = 9,
  height = 5.5,
  dpi = 300
)


# ---------------------------------------------------------------------
# 11. Completion message
# ---------------------------------------------------------------------

message(
  "Visualization completed successfully."
)

message(
  "Six figures written to output/figures/."
)