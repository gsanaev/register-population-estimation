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


# ---------------------------------------------------------------------
# Consolidate one or more attainment ranges within a reference year
# ---------------------------------------------------------------------

consolidate_same_year_attainment <- function(
  level_min,
  level_max
) {

  if (
    length(level_min) == 0L ||
    length(level_max) == 0L
  ) {
    stop(
      "At least one attainment observation is required.",
      call. = FALSE
    )
  }

  if (
    length(level_min) !=
      length(level_max)
  ) {
    stop(
      "Attainment minimum and maximum vectors must have equal length.",
      call. = FALSE
    )
  }

  for (
    observation_index in
      seq_along(level_min)
  ) {

    assert_valid_attainment_range(
      level_min[
        observation_index
      ],
      level_max[
        observation_index
      ]
    )
  }


  consolidated_min <-
    max(
      level_min
    )

  consolidated_max <-
    min(
      level_max
    )


  if (
    consolidated_min >
      consolidated_max
  ) {

    return(
      list(
        compatible = FALSE,
        level_min = NA_integer_,
        level_max = NA_integer_,
        decision_reason =
          "same_year_conflict"
      )
    )
  }


  list(
    compatible = TRUE,
    level_min =
      as.integer(
        consolidated_min
      ),
    level_max =
      as.integer(
        consolidated_max
      ),
    decision_reason =
      if (
        length(level_min) == 1L
      ) {
        "single_observation"
      } else {
        "same_year_compatible"
      }
  )
}


# ---------------------------------------------------------------------
# Resolve an attainment state across reference years
# ---------------------------------------------------------------------

resolve_attainment_state <- function(
  previous_min = NA_integer_,
  previous_max = NA_integer_,
  new_min = NA_integer_,
  new_max = NA_integer_,
  new_same_year_conflict = FALSE
) {

  if (
    length(new_same_year_conflict) != 1L ||
    is.na(new_same_year_conflict) ||
    !is.logical(new_same_year_conflict)
  ) {
    stop(
      "new_same_year_conflict must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  previous_missing <-
    is.na(previous_min) &&
    is.na(previous_max)

  new_missing <-
    is.na(new_min) &&
    is.na(new_max)


  if (
    xor(
      is.na(previous_min),
      is.na(previous_max)
    )
  ) {
    stop(
      "Previous attainment range must be fully present or fully missing.",
      call. = FALSE
    )
  }

  if (
    xor(
      is.na(new_min),
      is.na(new_max)
    )
  ) {
    stop(
      "New attainment range must be fully present or fully missing.",
      call. = FALSE
    )
  }


  if (!previous_missing) {
    assert_valid_attainment_range(
      previous_min,
      previous_max
    )
  }

  if (!new_missing) {
    assert_valid_attainment_range(
      new_min,
      new_max
    )
  }


  if (
    new_same_year_conflict &&
    !new_missing
  ) {
    stop(
      paste(
        "A same-year conflict cannot also contain",
        "a consolidated new attainment range."
      ),
      call. = FALSE
    )
  }


  # No usable evidence in either period
  if (
    previous_missing &&
    new_missing &&
    !new_same_year_conflict
  ) {
    return(
      list(
        status = "not_reported",
        level_min = NA_integer_,
        level_max = NA_integer_,
        decision_reason = "no_usable_evidence"
      )
    )
  }


  # Current-year conflict without an earlier accepted state
  if (
    previous_missing &&
    new_same_year_conflict
  ) {
    return(
      list(
        status = "review_required",
        level_min = NA_integer_,
        level_max = NA_integer_,
        decision_reason = "same_year_conflict"
      )
    )
  }


  # Current-year conflict with an earlier accepted state
  if (
    !previous_missing &&
    new_same_year_conflict
  ) {
    return(
      list(
        status = "review_required",
        level_min =
          as.integer(
            previous_min
          ),
        level_max =
          as.integer(
            previous_max
          ),
        decision_reason =
          "same_year_conflict_retained_prior"
      )
    )
  }


  # Earlier evidence exists but no later observation is available
  if (
    !previous_missing &&
    new_missing
  ) {
    return(
      list(
        status = "accepted",
        level_min =
          as.integer(
            previous_min
          ),
        level_max =
          as.integer(
            previous_max
          ),
        decision_reason = "carry_forward"
      )
    )
  }


  # First usable attainment evidence
  if (
    previous_missing &&
    !new_missing
  ) {
    return(
      list(
        status = "accepted",
        level_min =
          as.integer(
            new_min
          ),
        level_max =
          as.integer(
            new_max
          ),
        decision_reason = "initial_observation"
      )
    )
  }


  relation <-
    classify_attainment_relation(
      previous_min,
      previous_max,
      new_min,
      new_max,
      same_reference_year = FALSE
    )


  if (
    relation == "compatible"
  ) {

    refined_range <-
      intersect_attainment_ranges(
        previous_min,
        previous_max,
        new_min,
        new_max
      )

    retained_previous <-
      refined_range[["level_min"]] ==
        previous_min &&
      refined_range[["level_max"]] ==
        previous_max

    return(
      list(
        status = "accepted",
        level_min =
          as.integer(
            refined_range[["level_min"]]
          ),
        level_max =
          as.integer(
            refined_range[["level_max"]]
          ),
        decision_reason =
          if (
            retained_previous
          ) {
            "compatible_retained"
          } else {
            "compatible_refinement"
          }
      )
    )
  }


  if (
    relation ==
      "upward_progression"
  ) {
    return(
      list(
        status = "accepted",
        level_min =
          as.integer(
            new_min
          ),
        level_max =
          as.integer(
            new_max
          ),
        decision_reason =
          "upward_progression"
      )
    )
  }


  if (
    relation ==
      "temporal_regression"
  ) {
    return(
      list(
        status = "review_required",
        level_min =
          as.integer(
            previous_min
          ),
        level_max =
          as.integer(
            previous_max
          ),
        decision_reason =
          "temporal_regression"
      )
    )
  }


  stop(
    "Unable to resolve longitudinal attainment state.",
    call. = FALSE
  )
}
