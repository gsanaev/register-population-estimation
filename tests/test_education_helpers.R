# =====================================================================
# test_education_helpers.R
# Scenario tests for educational-attainment helper functions
# =====================================================================

source("R/education/education_helpers.R")


# Compatible precise and broad evidence
stopifnot(
  attainment_ranges_overlap(4, 4, 4, 6)
)


# Incompatible precise evidence
stopifnot(
  !attainment_ranges_overlap(5, 5, 4, 4)
)


# Identical ranges are compatible
stopifnot(
  attainment_ranges_overlap(3, 3, 3, 3)
)


# Invalid reversed range must fail explicitly
invalid_range <- try(
  assert_valid_attainment_range(5, 4),
  silent = TRUE
)

stopifnot(
  inherits(invalid_range, "try-error")
)


# Out-of-range attainment level must fail explicitly
invalid_level <- try(
  assert_valid_attainment_range(0, 3),
  silent = TRUE
)

stopifnot(
  inherits(invalid_level, "try-error")
)


# Broad evidence can be narrowed by more precise evidence
narrowed_range <- intersect_attainment_ranges(
  4,
  6,
  5,
  5
)

stopifnot(
  identical(
    unname(narrowed_range),
    c(5, 5)
  )
)


# Incompatible ranges cannot be intersected
invalid_intersection <- try(
  intersect_attainment_ranges(
    5,
    5,
    3,
    3
  ),
  silent = TRUE
)

stopifnot(
  inherits(invalid_intersection, "try-error")
)


# Bachelor followed by Master is upward progression
stopifnot(
  identical(
    classify_attainment_relation(
      4,
      4,
      5,
      5,
      same_reference_year = FALSE
    ),
    "upward_progression"
  )
)


# Master followed by Bachelor is temporal regression
stopifnot(
  identical(
    classify_attainment_relation(
      5,
      5,
      4,
      4,
      same_reference_year = FALSE
    ),
    "temporal_regression"
  )
)


# Same-year incompatible evidence requires review
stopifnot(
  identical(
    classify_attainment_relation(
      4,
      4,
      3,
      3,
      same_reference_year = TRUE
    ),
    "same_year_conflict"
  )
)


# Broad and precise evidence can remain compatible
stopifnot(
  identical(
    classify_attainment_relation(
      4,
      6,
      5,
      5,
      same_reference_year = TRUE
    ),
    "compatible"
  )
)


# Single same-year observation is retained unchanged
single_observation <-
  consolidate_same_year_attainment(
    4,
    4
  )

stopifnot(
  identical(
    single_observation$compatible,
    TRUE
  ),
  identical(
    single_observation$level_min,
    4L
  ),
  identical(
    single_observation$level_max,
    4L
  ),
  identical(
    single_observation$decision_reason,
    "single_observation"
  )
)


# Broad and precise compatible evidence are narrowed
compatible_same_year <-
  consolidate_same_year_attainment(
    c(
      4,
      5
    ),
    c(
      6,
      5
    )
  )

stopifnot(
  identical(
    compatible_same_year$compatible,
    TRUE
  ),
  identical(
    compatible_same_year$level_min,
    5L
  ),
  identical(
    compatible_same_year$level_max,
    5L
  ),
  identical(
    compatible_same_year$decision_reason,
    "same_year_compatible"
  )
)


# Compatible partially overlapping ranges use their common information
partially_overlapping_same_year <-
  consolidate_same_year_attainment(
    c(
      2,
      3
    ),
    c(
      3,
      3
    )
  )

stopifnot(
  identical(
    partially_overlapping_same_year$compatible,
    TRUE
  ),
  identical(
    partially_overlapping_same_year$level_min,
    3L
  ),
  identical(
    partially_overlapping_same_year$level_max,
    3L
  )
)


# Disjoint same-year evidence remains unresolved
same_year_conflict <-
  consolidate_same_year_attainment(
    c(
      2,
      4
    ),
    c(
      3,
      4
    )
  )

stopifnot(
  identical(
    same_year_conflict$compatible,
    FALSE
  ),
  is.na(
    same_year_conflict$level_min
  ),
  is.na(
    same_year_conflict$level_max
  ),
  identical(
    same_year_conflict$decision_reason,
    "same_year_conflict"
  )
)


# Mismatched vectors must fail explicitly
invalid_same_year_lengths <- try(
  consolidate_same_year_attainment(
    c(
      2,
      3
    ),
    3
  ),
  silent = TRUE
)

stopifnot(
  inherits(
    invalid_same_year_lengths,
    "try-error"
  )
)


# Empty same-year evidence must fail explicitly
empty_same_year <- try(
  consolidate_same_year_attainment(
    numeric(0),
    numeric(0)
  ),
  silent = TRUE
)

stopifnot(
  inherits(
    empty_same_year,
    "try-error"
  )
)


# First usable observation establishes an accepted state
initial_state <-
  resolve_attainment_state(
    new_min = 3,
    new_max = 3
  )

stopifnot(
  identical(
    initial_state$status,
    "accepted"
  ),
  identical(
    initial_state$level_min,
    3L
  ),
  identical(
    initial_state$level_max,
    3L
  ),
  identical(
    initial_state$decision_reason,
    "initial_observation"
  )
)


# Earlier attainment is carried forward when later evidence is absent
carried_forward <-
  resolve_attainment_state(
    previous_min = 4,
    previous_max = 4
  )

stopifnot(
  identical(
    carried_forward$status,
    "accepted"
  ),
  identical(
    carried_forward$level_min,
    4L
  ),
  identical(
    carried_forward$level_max,
    4L
  ),
  identical(
    carried_forward$decision_reason,
    "carry_forward"
  )
)


# Later broad compatible evidence does not weaken an established state
compatible_retained <-
  resolve_attainment_state(
    previous_min = 4,
    previous_max = 4,
    new_min = 4,
    new_max = 6
  )

stopifnot(
  identical(
    compatible_retained$status,
    "accepted"
  ),
  identical(
    compatible_retained$level_min,
    4L
  ),
  identical(
    compatible_retained$level_max,
    4L
  ),
  identical(
    compatible_retained$decision_reason,
    "compatible_retained"
  )
)


# Later precise compatible evidence can refine an earlier broad state
compatible_refinement <-
  resolve_attainment_state(
    previous_min = 4,
    previous_max = 6,
    new_min = 5,
    new_max = 5
  )

stopifnot(
  identical(
    compatible_refinement$status,
    "accepted"
  ),
  identical(
    compatible_refinement$level_min,
    5L
  ),
  identical(
    compatible_refinement$level_max,
    5L
  ),
  identical(
    compatible_refinement$decision_reason,
    "compatible_refinement"
  )
)


# Entirely higher later evidence is accepted as upward progression
progressed_state <-
  resolve_attainment_state(
    previous_min = 3,
    previous_max = 3,
    new_min = 4,
    new_max = 6
  )

stopifnot(
  identical(
    progressed_state$status,
    "accepted"
  ),
  identical(
    progressed_state$level_min,
    4L
  ),
  identical(
    progressed_state$level_max,
    6L
  ),
  identical(
    progressed_state$decision_reason,
    "upward_progression"
  )
)


# Later lower evidence is flagged while the earlier state is retained
regression_state <-
  resolve_attainment_state(
    previous_min = 5,
    previous_max = 5,
    new_min = 3,
    new_max = 3
  )

stopifnot(
  identical(
    regression_state$status,
    "review_required"
  ),
  identical(
    regression_state$level_min,
    5L
  ),
  identical(
    regression_state$level_max,
    5L
  ),
  identical(
    regression_state$decision_reason,
    "temporal_regression"
  )
)


# Same-year conflict without prior evidence remains unresolved
unresolved_conflict <-
  resolve_attainment_state(
    new_same_year_conflict = TRUE
  )

stopifnot(
  identical(
    unresolved_conflict$status,
    "review_required"
  ),
  is.na(
    unresolved_conflict$level_min
  ),
  is.na(
    unresolved_conflict$level_max
  ),
  identical(
    unresolved_conflict$decision_reason,
    "same_year_conflict"
  )
)


# Same-year conflict with prior evidence retains the earlier state
conflict_with_prior <-
  resolve_attainment_state(
    previous_min = 3,
    previous_max = 3,
    new_same_year_conflict = TRUE
  )

stopifnot(
  identical(
    conflict_with_prior$status,
    "review_required"
  ),
  identical(
    conflict_with_prior$level_min,
    3L
  ),
  identical(
    conflict_with_prior$level_max,
    3L
  ),
  identical(
    conflict_with_prior$decision_reason,
    "same_year_conflict_retained_prior"
  )
)


# No usable evidence produces not_reported rather than a low attainment
no_evidence_state <-
  resolve_attainment_state()

stopifnot(
  identical(
    no_evidence_state$status,
    "not_reported"
  ),
  is.na(
    no_evidence_state$level_min
  ),
  is.na(
    no_evidence_state$level_max
  ),
  identical(
    no_evidence_state$decision_reason,
    "no_usable_evidence"
  )
)


# Partially missing ranges must fail explicitly
partial_previous_range <- try(
  resolve_attainment_state(
    previous_min = 3,
    previous_max = NA
  ),
  silent = TRUE
)

stopifnot(
  inherits(
    partial_previous_range,
    "try-error"
  )
)


# A conflict cannot simultaneously contain a consolidated new range
conflict_with_range <- try(
  resolve_attainment_state(
    new_min = 3,
    new_max = 3,
    new_same_year_conflict = TRUE
  ),
  silent = TRUE
)

stopifnot(
  inherits(
    conflict_with_range,
    "try-error"
  )
)


message("Education helper tests passed successfully.")
