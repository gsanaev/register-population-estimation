# =====================================================================
# education_helpers.R
# Helper functions for synthetic educational-attainment integration
# ---------------------------------------------------------------------
# The harmonised attainment scale used in this extension is deliberately
# simplified and synthetic. It does not reproduce an official Destatis
# or ISCED classification procedure.
# =====================================================================


# ---------------------------------------------------------------------
# Validate an attainment-level range
# ---------------------------------------------------------------------

assert_valid_attainment_range <- function(level_min, level_max) {

  if (
    length(level_min) != 1L ||
    length(level_max) != 1L ||
    is.na(level_min) ||
    is.na(level_max)
  ) {
    stop(
      "Attainment ranges must contain two non-missing scalar values.",
      call. = FALSE
    )
  }

  if (
    !level_min %in% 1:6 ||
    !level_max %in% 1:6
  ) {
    stop(
      "Attainment levels must lie between 1 and 6.",
      call. = FALSE
    )
  }

  if (level_min > level_max) {
    stop(
      "Attainment range minimum cannot exceed maximum.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------
# Test whether two attainment ranges are compatible
# ---------------------------------------------------------------------

attainment_ranges_overlap <- function(
  level_min_a,
  level_max_a,
  level_min_b,
  level_max_b
) {

  assert_valid_attainment_range(
    level_min_a,
    level_max_a
  )

  assert_valid_attainment_range(
    level_min_b,
    level_max_b
  )

  max(level_min_a, level_min_b) <=
    min(level_max_a, level_max_b)
}


# ---------------------------------------------------------------------
# Narrow two compatible ranges to their common information
# ---------------------------------------------------------------------

intersect_attainment_ranges <- function(
  level_min_a,
  level_max_a,
  level_min_b,
  level_max_b
) {

  if (
    !attainment_ranges_overlap(
      level_min_a,
      level_max_a,
      level_min_b,
      level_max_b
    )
  ) {
    stop(
      "Cannot intersect incompatible attainment ranges.",
      call. = FALSE
    )
  }

  c(
    level_min = max(level_min_a, level_min_b),
    level_max = min(level_max_a, level_max_b)
  )
}


# ---------------------------------------------------------------------
# Classify the relationship between two attainment observations
# ---------------------------------------------------------------------

classify_attainment_relation <- function(
  previous_min,
  previous_max,
  new_min,
  new_max,
  same_reference_year = FALSE
) {

  assert_valid_attainment_range(
    previous_min,
    previous_max
  )

  assert_valid_attainment_range(
    new_min,
    new_max
  )

  if (
    length(same_reference_year) != 1L ||
    is.na(same_reference_year) ||
    !is.logical(same_reference_year)
  ) {
    stop(
      "same_reference_year must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    attainment_ranges_overlap(
      previous_min,
      previous_max,
      new_min,
      new_max
    )
  ) {
    return("compatible")
  }

  if (same_reference_year) {
    return("same_year_conflict")
  }

  if (new_min > previous_max) {
    return("upward_progression")
  }

  if (new_max < previous_min) {
    return("temporal_regression")
  }

  stop(
    "Unable to classify attainment relationship.",
    call. = FALSE
  )
}
