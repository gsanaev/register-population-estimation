# =====================================================================
# 01_validate_harmonize_education.R
# Validate and Harmonise Synthetic Educational-Attainment Deliveries
# ---------------------------------------------------------------------
# This workflow performs delivery-level validation, record-level quality
# checks, and harmonisation of heterogeneous synthetic education sources.
#
# It deliberately does not use synthetic_education_truth.csv. Hidden
# truth is reserved for later evaluation and is not part of operational
# validation, harmonisation, or reconciliation.
#
# Inputs:
#   data/education/raw/zensus_2022_like_delivery.csv
#   data/education/raw/ba_2024_like_delivery.csv
#   data/education/raw/mikrozensus_2024_like_delivery.csv
#
# Outputs:
#   data/education/clean/education_evidence_harmonized.csv
#   data/education/clean/education_evidence_usable.csv
#   output/education/tables/education_delivery_qa_summary.csv
# =====================================================================


# ---------------------------------------------------------------------
# 1. Load packages
# ---------------------------------------------------------------------

library(dplyr)
library(readr)


# ---------------------------------------------------------------------
# 2. Define input and output paths
# ---------------------------------------------------------------------

zensus_path <-
  "data/education/raw/zensus_2022_like_delivery.csv"

ba_path <-
  "data/education/raw/ba_2024_like_delivery.csv"

mikrozensus_path <-
  "data/education/raw/mikrozensus_2024_like_delivery.csv"

education_clean_dir <-
  "data/education/clean"

education_output_tables_dir <-
  "output/education/tables"


dir.create(
  education_clean_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  education_output_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------------------------------------------------
# 3. Validate availability of required source deliveries
# ---------------------------------------------------------------------

required_input_files <- c(
  zensus_path,
  ba_path,
  mikrozensus_path
)

missing_input_files <-
  required_input_files[
    !file.exists(
      required_input_files
    )
  ]

if (length(missing_input_files) > 0L) {
  stop(
    paste(
      "Required education delivery files are missing:",
      paste(
        missing_input_files,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 4. Load raw deliveries
# ---------------------------------------------------------------------

zensus_raw <- read_csv(
  zensus_path,
  show_col_types = FALSE
)

ba_raw <- read_csv(
  ba_path,
  show_col_types = FALSE
)

mikrozensus_raw <- read_csv(
  mikrozensus_path,
  show_col_types = FALSE
)


# ---------------------------------------------------------------------
# 5. Validate required delivery schemas
# ---------------------------------------------------------------------

assert_required_columns <- function(
  data,
  required_columns,
  source_name
) {

  missing_columns <-
    setdiff(
      required_columns,
      names(data)
    )

  if (length(missing_columns) > 0L) {
    stop(
      paste(
        source_name,
        "is missing required columns:",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


assert_required_columns(
  zensus_raw,
  c(
    "person_id",
    "reference_year",
    "highest_qualification"
  ),
  "Zensus-like delivery"
)

assert_required_columns(
  ba_raw,
  c(
    "person_id",
    "reporting_year",
    "qualification_group"
  ),
  "BA-like delivery"
)

assert_required_columns(
  mikrozensus_raw,
  c(
    "person_id",
    "survey_year",
    "education_code"
  ),
  "Mikrozensus-like delivery"
)


message(
  "Education delivery files and schemas validated successfully."
)


# ---------------------------------------------------------------------
# 6. Standardise source-delivery fields
# ---------------------------------------------------------------------
# Source-specific field names are converted to a common record structure
# before applying shared quality checks. source_row_id preserves the
# original row position within each delivery for traceability.

zensus_standardised <-
  zensus_raw %>%

  mutate(
    source_row_id = row_number()
  ) %>%

  transmute(
    source = "zensus_like_2022",
    source_row_id,
    person_id,
    reference_year =
      as.integer(
        reference_year
      ),
    source_code =
      highest_qualification
  )


ba_standardised <-
  ba_raw %>%

  mutate(
    source_row_id = row_number()
  ) %>%

  transmute(
    source = "ba_like_2024",
    source_row_id,
    person_id,
    reference_year =
      as.integer(
        reporting_year
      ),
    source_code =
      qualification_group
  )


mikrozensus_standardised <-
  mikrozensus_raw %>%

  mutate(
    source_row_id = row_number()
  ) %>%

  transmute(
    source = "mikrozensus_like_2024",
    source_row_id,
    person_id,
    reference_year =
      as.integer(
        survey_year
      ),
    source_code =
      education_code
  )


# ---------------------------------------------------------------------
# 7. Derive common structural QA flags
# ---------------------------------------------------------------------

add_structural_qa_flags <- function(
  data,
  expected_year
) {

  duplicate_keys <-
    data %>%

    filter(
      !is.na(person_id)
    ) %>%

    count(
      person_id,
      reference_year
    ) %>%

    filter(
      n > 1L
    ) %>%

    select(
      person_id,
      reference_year
    ) %>%

    mutate(
      duplicate_person_year = TRUE
    )


  data %>%

    left_join(
      duplicate_keys,
      by = c(
        "person_id",
        "reference_year"
      )
    ) %>%

    mutate(
      duplicate_person_year =
        coalesce(
          duplicate_person_year,
          FALSE
        ),

      missing_person_id =
        is.na(
          person_id
        ),

      invalid_reference_year =
        is.na(
          reference_year
        ) |
        reference_year !=
          expected_year,

      missing_attainment =
        is.na(
          source_code
        )
    )
}


zensus_structural_qa <-
  add_structural_qa_flags(
    zensus_standardised,
    expected_year = 2022L
  )


ba_structural_qa <-
  add_structural_qa_flags(
    ba_standardised,
    expected_year = 2024L
  )


mikrozensus_structural_qa <-
  add_structural_qa_flags(
    mikrozensus_standardised,
    expected_year = 2024L
  )


education_structural_qa <-
  bind_rows(
    zensus_structural_qa,
    ba_structural_qa,
    mikrozensus_structural_qa
  )


if (
  anyDuplicated(
    education_structural_qa[
      c(
        "source",
        "source_row_id"
      )
    ]
  ) > 0L
) {
  stop(
    paste(
      "Structural QA data contains duplicated",
      "source/source_row_id identifiers."
    ),
    call. = FALSE
  )
}


expected_structural_rows <-
  nrow(zensus_raw) +
  nrow(ba_raw) +
  nrow(mikrozensus_raw)

if (
  nrow(
    education_structural_qa
  ) !=
    expected_structural_rows
) {
  stop(
    "Structural QA changed the number of delivered records.",
    call. = FALSE
  )
}


message(
  "Common structural education QA completed successfully."
)


# ---------------------------------------------------------------------
# 8. Define source-specific attainment mappings
# ---------------------------------------------------------------------

education_codebook <- bind_rows(

  tibble(
    source = "zensus_like_2022",
    source_code = c(
      "NONE_LOW",
      "SCHOOL",
      "VOC_POSTSEC",
      "BACHELOR_EQ",
      "MASTER_EQ",
      "DOCTORATE"
    ),
    level_min = 1:6,
    level_max = 1:6
  ),

  tibble(
    source = "ba_like_2024",
    source_code = c(
      "LOW_NONE",
      "SCHOOL_VOC",
      "HIGHER_ED"
    ),
    level_min = c(
      1L,
      2L,
      4L
    ),
    level_max = c(
      1L,
      3L,
      6L
    )
  ),

  tibble(
    source = "mikrozensus_like_2024",
    source_code = c(
      "E1",
      "E2",
      "E3",
      "E4",
      "E5",
      "E6"
    ),
    level_min = 1:6,
    level_max = 1:6
  )
)


if (
  anyDuplicated(
    education_codebook[
      c(
        "source",
        "source_code"
      )
    ]
  ) > 0L
) {
  stop(
    "Education codebook contains duplicated source/code mappings.",
    call. = FALSE
  )
}

if (
  any(
    education_codebook$level_min < 1L |
      education_codebook$level_max > 6L |
      education_codebook$level_min >
        education_codebook$level_max
  )
) {
  stop(
    "Education codebook contains invalid attainment ranges.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 9. Harmonise source codes into attainment ranges
# ---------------------------------------------------------------------

education_harmonized <-
  education_structural_qa %>%

  left_join(
    education_codebook,
    by = c(
      "source",
      "source_code"
    )
  ) %>%

  mutate(
    unknown_source_code =
      !missing_attainment &
      (
        is.na(level_min) |
          is.na(level_max)
      )
  )


if (
  nrow(
    education_harmonized
  ) !=
    nrow(
      education_structural_qa
    )
) {
  stop(
    "Code harmonisation changed the number of delivered records.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 10. Assign final record-level QA status and reason
# ---------------------------------------------------------------------
# Rejection reasons take precedence over flagged missing attainment.
# This ensures that a structurally invalid record is never retained
# merely because another field is also missing.

education_harmonized <-
  education_harmonized %>%

  mutate(
    qa_status = case_when(
      missing_person_id ~
        "rejected",

      duplicate_person_year ~
        "rejected",

      invalid_reference_year ~
        "rejected",

      unknown_source_code ~
        "rejected",

      missing_attainment ~
        "flagged",

      TRUE ~
        "usable"
    ),

    qa_reason = case_when(
      missing_person_id ~
        "missing_person_id",

      duplicate_person_year ~
        "duplicate_person_year",

      invalid_reference_year ~
        "invalid_reference_year",

      unknown_source_code ~
        "unknown_source_code",

      missing_attainment ~
        "missing_attainment",

      TRUE ~
        "valid_evidence"
    )
  ) %>%

  select(
    source,
    source_row_id,
    person_id,
    reference_year,
    source_code,
    level_min,
    level_max,
    qa_status,
    qa_reason
  )


# ---------------------------------------------------------------------
# 11. Validate harmonised QA results
# ---------------------------------------------------------------------

if (
  any(
    !education_harmonized$
      qa_status %in%
      c(
        "usable",
        "flagged",
        "rejected"
      )
  )
) {
  stop(
    "Unexpected education QA status.",
    call. = FALSE
  )
}

if (
  any(
    education_harmonized$
      qa_status == "usable" &
      (
        is.na(
          education_harmonized$
            level_min
        ) |
          is.na(
            education_harmonized$
              level_max
          )
      )
  )
) {
  stop(
    "Usable education evidence contains missing attainment ranges.",
    call. = FALSE
  )
}

if (
  any(
    education_harmonized$
      qa_status != "usable" &
      !is.na(
        education_harmonized$
          level_min
      ) &
      education_harmonized$
        qa_reason %in%
        c(
          "missing_attainment",
          "unknown_source_code"
        )
  )
) {
  stop(
    paste(
      "Missing or unknown attainment records",
      "unexpectedly contain harmonised ranges."
    ),
    call. = FALSE
  )
}


message(
  "Education code harmonisation and QA classification completed successfully."
)


# ---------------------------------------------------------------------
# 12. Create usable evidence and delivery QA summary
# ---------------------------------------------------------------------

education_usable <-
  education_harmonized %>%

  filter(
    qa_status == "usable"
  )


education_delivery_qa_summary <-
  education_harmonized %>%

  count(
    source,
    qa_status,
    qa_reason,
    name = "n_records"
  ) %>%

  group_by(
    source
  ) %>%

  mutate(
    source_total_records =
      sum(
        n_records
      ),

    share_of_source_records =
      n_records /
      source_total_records
  ) %>%

  ungroup() %>%

  arrange(
    source,
    factor(
      qa_status,
      levels = c(
        "usable",
        "flagged",
        "rejected"
      )
    ),
    qa_reason
  )


# ---------------------------------------------------------------------
# 13. Validate final operational outputs
# ---------------------------------------------------------------------

if (
  nrow(
    education_harmonized
  ) !=
    expected_structural_rows
) {
  stop(
    "Final harmonised evidence does not preserve all delivered rows.",
    call. = FALSE
  )
}

if (
  any(
    education_usable$
      qa_status != "usable"
  )
) {
  stop(
    "Usable education evidence contains non-usable QA statuses.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      education_usable$
        person_id
    )
  )
) {
  stop(
    "Usable education evidence contains missing person_id values.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      education_usable$
        level_min
    ) |
      is.na(
        education_usable$
          level_max
      )
  )
) {
  stop(
    "Usable education evidence contains missing attainment ranges.",
    call. = FALSE
  )
}

if (
  any(
    education_usable$
      level_min >
      education_usable$
        level_max
  )
) {
  stop(
    "Usable education evidence contains invalid attainment ranges.",
    call. = FALSE
  )
}

if (
  sum(
    education_delivery_qa_summary$
      n_records
  ) !=
    nrow(
      education_harmonized
    )
) {
  stop(
    "Delivery QA summary does not reconcile to harmonised evidence.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------
# 14. Write harmonised evidence and QA outputs
# ---------------------------------------------------------------------

write_csv(
  education_harmonized,
  file.path(
    education_clean_dir,
    "education_evidence_harmonized.csv"
  ),
  na = ""
)

write_csv(
  education_usable,
  file.path(
    education_clean_dir,
    "education_evidence_usable.csv"
  ),
  na = ""
)

write_csv(
  education_delivery_qa_summary,
  file.path(
    education_output_tables_dir,
    "education_delivery_qa_summary.csv"
  )
)


message(
  "Education validation and harmonisation outputs written successfully."
)

message(
  "Delivered records: ",
  nrow(
    education_harmonized
  )
)

message(
  "Usable evidence records: ",
  nrow(
    education_usable
  )
)

message(
  "Flagged records: ",
  sum(
    education_harmonized$
      qa_status == "flagged"
  )
)

message(
  "Rejected records: ",
  sum(
    education_harmonized$
      qa_status == "rejected"
  )
)
