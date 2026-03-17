# =====================================================================
# 05_visualize_results.R
# Visualization of Register-Based Population Estimation Results
# ---------------------------------------------------------------------
# This script creates core figures for the synthetic register-based
# population estimation workflow.
#
# It visualizes:
#   - distribution of activity signals
#   - estimated overcoverage by age group
#   - register population vs likely residents by region
#   - estimation error by region
#
# Output:
#   output/figures/signal_count_distribution.png
#   output/figures/overcoverage_rate_by_age_group.png
#   output/figures/register_vs_likely_residents_by_region.png
#   output/figures/estimation_error_by_region.png
# =====================================================================

# ---------------------------------------------------------------------
# 0. Load packages
# ---------------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)

# ---------------------------------------------------------------------
# 1. Ensure output directory exists
# ---------------------------------------------------------------------
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 2. Load data
# ---------------------------------------------------------------------
person_register <- read_csv(
  "data/processed/person_register_integrated.csv",
  show_col_types = FALSE
) %>%
  clean_names()

population_by_region <- read_csv(
  "output/tables/population_estimation_by_region.csv",
  show_col_types = FALSE
) %>%
  clean_names()

population_by_age_group <- read_csv(
  "output/tables/population_estimation_by_age_group.csv",
  show_col_types = FALSE
) %>%
  clean_names()

# ---------------------------------------------------------------------
# 3. Plot: Distribution of activity signal counts
# ---------------------------------------------------------------------
p_signal_count <- person_register %>%
  ggplot(aes(x = factor(signal_count))) +
  geom_bar() +
  labs(
    title = "Distribution of Activity Signal Counts",
    x = "Number of Activity Signals",
    y = "Number of Registered Persons"
  )

ggsave(
  "output/figures/signal_count_distribution.png",
  p_signal_count,
  width = 7,
  height = 5
)

# ---------------------------------------------------------------------
# 4. Plot: Estimated overcoverage rate by age group
# ---------------------------------------------------------------------
p_overcoverage_age <- population_by_age_group %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c("0-5", "6-17", "18-24", "25-39", "40-64", "65-79", "80+")
    )
  ) %>%
  ggplot(aes(x = age_group, y = estimated_overcoverage_rate)) +
  geom_col() +
  labs(
    title = "Estimated Overcoverage Rate by Age Group",
    x = "Age Group",
    y = "Estimated Overcoverage Rate"
  )

ggsave(
  "output/figures/overcoverage_rate_by_age_group.png",
  p_overcoverage_age,
  width = 7,
  height = 5
)

# ---------------------------------------------------------------------
# 5. Plot: Register population vs likely residents by region
# ---------------------------------------------------------------------
region_long <- population_by_region %>%
  select(region_code, register_population_count, likely_resident_count) %>%
  tidyr::pivot_longer(
    cols = c(register_population_count, likely_resident_count),
    names_to = "population_type",
    values_to = "count"
  )

p_region_comparison <- region_long %>%
  ggplot(aes(x = region_code, y = count, fill = population_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Register Population vs Likely Residents by Region",
    x = "Region",
    y = "Population Count",
    fill = "Population Type"
  )

ggsave(
  "output/figures/register_vs_likely_residents_by_region.png",
  p_region_comparison,
  width = 8,
  height = 5
)

# ---------------------------------------------------------------------
# 6. Plot: Estimation error by region
# ---------------------------------------------------------------------
p_estimation_error <- population_by_region %>%
  ggplot(aes(x = region_code, y = estimation_error_rate)) +
  geom_col() +
  labs(
    title = "Estimation Error Rate by Region",
    x = "Region",
    y = "Estimation Error Rate"
  )

ggsave(
  "output/figures/estimation_error_by_region.png",
  p_estimation_error,
  width = 8,
  height = 5
)

message("Figures written to 'output/figures/'.")