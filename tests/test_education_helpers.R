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


message("Education helper tests passed successfully.")
