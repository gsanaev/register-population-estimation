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
