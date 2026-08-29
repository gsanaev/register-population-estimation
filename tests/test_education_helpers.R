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


message("Education helper tests passed successfully.")
