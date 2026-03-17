# =====================================================================
# 04_estimate_population_stock.R
# Population Stock Estimation and Quality Indicators
# ---------------------------------------------------------------------
# This script loads the integrated person-level register dataset and
# computes population-estimation indicators for a synthetic
# register-based population system.
#
# It produces:
#   - overall population stock estimates
#   - region-level estimates
#   - age-group estimates
#   - a simple quality comparison between rule-based estimates and
#     synthetic ground truth
#
# Output:
#   output/tables/population_estimation_overall.csv
#   output/tables/population_estimation_by_region.csv
#   output/tables/population_estimation_by_age_group.csv
#   output/tables/estimation_quality_summary.csv
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
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 2. Load processed datasets
# ---------------------------------------------------------------------
person_register <- read_csv(
  "data/processed/person_register_integrated.csv",
  show_col_types = FALSE
)

region_activity_summary <- read_csv(
  "data/processed/region_activity_summary.csv",
  show_col_types = FALSE
)

person_register <- person_register %>%
  clean_names()

message("Integrated person register loaded: ", nrow(person_register), " rows.")

# ---------------------------------------------------------------------
# 3. Overall population-estimation indicators
# ---------------------------------------------------------------------
message("Computing overall population-estimation indicators...")

population_estimation_overall <- person_register %>%
  summarise(
    register_population_count = n(),
    likely_resident_count = sum(likely_resident_rule == 1, na.rm = TRUE),
    estimated_overcoverage_count = sum(likely_resident_rule == 0, na.rm = TRUE),
    estimated_overcoverage_rate = estimated_overcoverage_count / register_population_count,

    true_resident_count = sum(resident_true == 1, na.rm = TRUE),
    true_overcoverage_count = sum(overcoverage_flag_true == 1, na.rm = TRUE),
    true_overcoverage_rate = true_overcoverage_count / register_population_count,

    estimation_error_count = likely_resident_count - true_resident_count,
    estimation_error_rate = estimation_error_count / register_population_count,

    no_activity_count = sum(flag_no_activity_signal, na.rm = TRUE),
    no_activity_rate = mean(flag_no_activity_signal, na.rm = TRUE),

    mean_signal_count = mean(signal_count, na.rm = TRUE)
  )

# ---------------------------------------------------------------------
# 4. Region-level population-estimation indicators
# ---------------------------------------------------------------------
message("Computing region-level indicators...")

population_estimation_by_region <- person_register %>%
  group_by(region_code) %>%
  summarise(
    register_population_count = n(),
    likely_resident_count = sum(likely_resident_rule == 1, na.rm = TRUE),
    estimated_overcoverage_count = sum(likely_resident_rule == 0, na.rm = TRUE),
    estimated_overcoverage_rate = estimated_overcoverage_count / register_population_count,

    true_resident_count = sum(resident_true == 1, na.rm = TRUE),
    true_overcoverage_count = sum(overcoverage_flag_true == 1, na.rm = TRUE),
    true_overcoverage_rate = true_overcoverage_count / register_population_count,

    estimation_error_count = likely_resident_count - true_resident_count,
    estimation_error_rate = estimation_error_count / register_population_count,

    no_activity_count = sum(flag_no_activity_signal, na.rm = TRUE),
    no_activity_rate = mean(flag_no_activity_signal, na.rm = TRUE),

    mean_signal_count = mean(signal_count, na.rm = TRUE),
    high_risk_count = sum(overcoverage_risk == "high", na.rm = TRUE),
    high_risk_rate = mean(overcoverage_risk == "high", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region_code)

# ---------------------------------------------------------------------
# 5. Age-group population-estimation indicators
# ---------------------------------------------------------------------
message("Computing age-group indicators...")

population_estimation_by_age_group <- person_register %>%
  group_by(age_group) %>%
  summarise(
    register_population_count = n(),
    likely_resident_count = sum(likely_resident_rule == 1, na.rm = TRUE),
    estimated_overcoverage_count = sum(likely_resident_rule == 0, na.rm = TRUE),
    estimated_overcoverage_rate = estimated_overcoverage_count / register_population_count,

    true_resident_count = sum(resident_true == 1, na.rm = TRUE),
    true_overcoverage_count = sum(overcoverage_flag_true == 1, na.rm = TRUE),
    true_overcoverage_rate = true_overcoverage_count / register_population_count,

    estimation_error_count = likely_resident_count - true_resident_count,
    estimation_error_rate = estimation_error_count / register_population_count,

    no_activity_count = sum(flag_no_activity_signal, na.rm = TRUE),
    no_activity_rate = mean(flag_no_activity_signal, na.rm = TRUE),

    mean_signal_count = mean(signal_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_group)

# ---------------------------------------------------------------------
# 6. Simple estimation-quality summary
# ---------------------------------------------------------------------
message("Computing estimation-quality summary...")

estimation_quality_summary <- person_register %>%
  mutate(
    predicted_resident = if_else(likely_resident_rule == 1, 1L, 0L),
    actual_resident = if_else(resident_true == 1, 1L, 0L)
  ) %>%
  summarise(
    true_positive  = sum(predicted_resident == 1 & actual_resident == 1, na.rm = TRUE),
    false_positive = sum(predicted_resident == 1 & actual_resident == 0, na.rm = TRUE),
    true_negative  = sum(predicted_resident == 0 & actual_resident == 0, na.rm = TRUE),
    false_negative = sum(predicted_resident == 0 & actual_resident == 1, na.rm = TRUE)
  ) %>%
  mutate(
    accuracy  = (true_positive + true_negative) /
      (true_positive + false_positive + true_negative + false_negative),
    precision = true_positive / (true_positive + false_positive),
    recall    = true_positive / (true_positive + false_negative),
    specificity = true_negative / (true_negative + false_positive)
  )

# ---------------------------------------------------------------------
# 7. Optional consistency messages
# ---------------------------------------------------------------------
message("Overall estimation summary:")
print(population_estimation_overall)

message("Quality summary:")
print(estimation_quality_summary)

# ---------------------------------------------------------------------
# 8. Write output tables
# ---------------------------------------------------------------------
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

message("Population estimation tables written to 'output/tables/'.")