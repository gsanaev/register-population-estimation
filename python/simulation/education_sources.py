"""Generate heterogeneous synthetic educational-attainment sources."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd


ZENSUS_2022_COLUMNS = [
    "person_id",
    "reference_year",
    "highest_qualification",
]

ZENSUS_TRUTH_REQUIRED_COLUMNS = {
    "person_id",
    "reference_year",
    "age_at_reference_year",
    "true_attainment_level",
}


def _validate_probability(
    value: Any,
    name: str,
) -> float:
    """Validate a scalar probability."""

    probability = float(value)

    if not 0.0 <= probability <= 1.0:
        raise ValueError(
            f"{name} must be between 0 and 1."
        )

    return probability


def _get_source_config(
    config: Mapping[str, Any],
    source_name: str,
) -> Mapping[str, Any]:
    """Return one education-source configuration section."""

    education_config = config.get(
        "education"
    )

    if not isinstance(
        education_config,
        Mapping,
    ):
        raise ValueError(
            "Configuration must contain an education section."
        )

    source_config = education_config.get(
        source_name
    )

    if not isinstance(
        source_config,
        Mapping,
    ):
        raise ValueError(
            f"education must contain a {source_name} section."
        )

    return source_config


def _minimum_age_by_level(
    config: Mapping[str, Any],
) -> np.ndarray:
    """Return validated minimum ages for levels 1-6."""

    education_config = config.get(
        "education"
    )

    if not isinstance(
        education_config,
        Mapping,
    ):
        raise ValueError(
            "Configuration must contain an education section."
        )

    truth_config = education_config.get(
        "truth"
    )

    if not isinstance(
        truth_config,
        Mapping,
    ):
        raise ValueError(
            "education must contain a truth section."
        )

    minimum_ages = np.asarray(
        truth_config.get(
            "minimum_age_by_level"
        ),
        dtype=np.int64,
    )

    if minimum_ages.shape != (6,):
        raise ValueError(
            "minimum_age_by_level must contain six values."
        )

    return minimum_ages


def _allocate_disjoint_indices(
    n_rows: int,
    counts: Mapping[str, int],
    rng: np.random.Generator,
) -> dict[str, np.ndarray]:
    """Allocate mutually disjoint row positions in a stable order."""

    if n_rows < 0:
        raise ValueError(
            "n_rows must be non-negative."
        )

    if any(
        not isinstance(count, int)
        or count < 0
        for count in counts.values()
    ):
        raise ValueError(
            "Defect counts must be non-negative integers."
        )

    if sum(counts.values()) > n_rows:
        raise ValueError(
            "Requested defect counts exceed available rows."
        )

    available = np.arange(
        n_rows,
        dtype=np.int64,
    )

    allocated: dict[
        str,
        np.ndarray,
    ] = {}

    for name, count in counts.items():
        if count == 0:
            selected = np.empty(
                0,
                dtype=np.int64,
            )
        else:
            selected = rng.choice(
                available,
                size=count,
                replace=False,
            ).astype(
                np.int64
            )

        allocated[name] = selected

        if count > 0:
            available = available[
                ~np.isin(
                    available,
                    selected,
                )
            ]

    return allocated


def _sample_alternative_attainment(
    true_level: int,
    age_value: int,
    minimum_ages: np.ndarray,
    rng: np.random.Generator,
) -> int:
    """Sample the nearest age-plausible level different from truth."""

    if not 1 <= true_level <= 6:
        raise ValueError(
            "true_level must be between 1 and 6."
        )

    allowed_levels = (
        np.flatnonzero(
            age_value
            >= minimum_ages
        )
        + 1
    )

    candidate_levels = allowed_levels[
        allowed_levels != true_level
    ]

    if len(candidate_levels) == 0:
        raise ValueError(
            "No alternative attainment level is available."
        )

    distances = np.abs(
        candidate_levels
        - true_level
    )

    nearest = candidate_levels[
        distances
        == distances.min()
    ]

    return int(
        rng.choice(
            nearest
        )
    )


def generate_zensus_2022_delivery(
    education_truth: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate the detailed synthetic Zensus-like 2022 delivery."""

    missing_columns = (
        ZENSUS_TRUTH_REQUIRED_COLUMNS
        - set(education_truth.columns)
    )

    if missing_columns:
        raise ValueError(
            "Education truth is missing Zensus fields: "
            + ", ".join(
                sorted(missing_columns)
            )
        )

    if education_truth.duplicated(
        subset=[
            "person_id",
            "reference_year",
        ]
    ).any():
        raise ValueError(
            "Education truth contains duplicated person-year records."
        )

    source_config = _get_source_config(
        config,
        "zensus_2022",
    )

    reference_year = source_config.get(
        "reference_year"
    )
    invalid_reference_year = source_config.get(
        "invalid_reference_year"
    )

    if (
        not isinstance(reference_year, int)
        or not isinstance(
            invalid_reference_year,
            int,
        )
    ):
        raise ValueError(
            "Zensus reference years must be integers."
        )

    coverage = _validate_probability(
        source_config.get(
            "coverage"
        ),
        "zensus coverage",
    )

    rates = {
        "measurement_error":
            _validate_probability(
                source_config.get(
                    "measurement_error_rate"
                ),
                "zensus measurement_error_rate",
            ),
        "missing_qualification":
            _validate_probability(
                source_config.get(
                    "missing_qualification_rate"
                ),
                "zensus missing_qualification_rate",
            ),
        "unknown_code":
            _validate_probability(
                source_config.get(
                    "unknown_code_rate"
                ),
                "zensus unknown_code_rate",
            ),
        "invalid_year":
            _validate_probability(
                source_config.get(
                    "invalid_year_rate"
                ),
                "zensus invalid_year_rate",
            ),
        "missing_person_id":
            _validate_probability(
                source_config.get(
                    "missing_person_id_rate"
                ),
                "zensus missing_person_id_rate",
            ),
        "duplicate_records":
            _validate_probability(
                source_config.get(
                    "duplicate_rate"
                ),
                "zensus duplicate_rate",
            ),
    }

    codes = tuple(
        source_config.get(
            "qualification_codes",
            (),
        )
    )

    if (
        len(codes) != 6
        or len(set(codes)) != 6
        or any(
            not isinstance(code, str)
            or not code
            for code in codes
        )
    ):
        raise ValueError(
            "Zensus qualification_codes must contain "
            "six unique non-empty strings."
        )

    truth_2022 = (
        education_truth.loc[
            education_truth[
                "reference_year"
            ].eq(reference_year)
        ]
        .reset_index(drop=True)
        .copy()
    )

    if truth_2022.empty:
        raise ValueError(
            "Education truth contains no Zensus reference-year rows."
        )

    if truth_2022[
        "person_id"
    ].isna().any():
        raise ValueError(
            "Zensus truth contains missing person IDs."
        )

    if truth_2022[
        "person_id"
    ].duplicated().any():
        raise ValueError(
            "Zensus truth contains duplicated person IDs."
        )

    true_levels_all = truth_2022[
        "true_attainment_level"
    ].to_numpy(
        dtype=np.int64
    )

    if (
        np.any(true_levels_all < 1)
        or np.any(true_levels_all > 6)
    ):
        raise ValueError(
            "Zensus truth contains invalid attainment levels."
        )

    n_base = int(
        np.floor(
            len(truth_2022)
            * coverage
        )
    )

    if n_base <= 0:
        raise ValueError(
            "Zensus coverage produces no source records."
        )

    sampled_positions = rng.choice(
        len(truth_2022),
        size=n_base,
        replace=False,
    )

    working = (
        truth_2022.iloc[
            sampled_positions
        ]
        .reset_index(drop=True)
        .copy()
    )

    true_levels = working[
        "true_attainment_level"
    ].to_numpy(
        dtype=np.int64
    )

    observed_levels = (
        true_levels.copy()
    )

    defect_counts = {
        name: int(
            np.floor(
                n_base
                * rate
            )
        )
        for name, rate in rates.items()
    }

    defect_indices = (
        _allocate_disjoint_indices(
            n_base,
            defect_counts,
            rng,
        )
    )

    minimum_ages = (
        _minimum_age_by_level(
            config
        )
    )

    ages = working[
        "age_at_reference_year"
    ].to_numpy(
        dtype=np.int64
    )

    for position in defect_indices[
        "measurement_error"
    ]:
        observed_levels[
            position
        ] = _sample_alternative_attainment(
            true_level=int(
                true_levels[
                    position
                ]
            ),
            age_value=int(
                ages[
                    position
                ]
            ),
            minimum_ages=minimum_ages,
            rng=rng,
        )

    realised_measurement_errors = int(
        (
            observed_levels
            != true_levels
        ).sum()
    )

    if (
        realised_measurement_errors
        != defect_counts[
            "measurement_error"
        ]
    ):
        raise RuntimeError(
            "Unexpected number of realised "
            "Zensus-like measurement errors."
        )

    code_values = np.asarray(
        codes,
        dtype=object,
    )[
        observed_levels - 1
    ]

    delivery = pd.DataFrame(
        {
            "person_id":
                working[
                    "person_id"
                ].to_numpy(
                    dtype=object
                ),
            "reference_year":
                np.full(
                    n_base,
                    reference_year,
                    dtype=np.int64,
                ),
            "highest_qualification":
                code_values,
        }
    )

    delivery.loc[
        defect_indices[
            "missing_qualification"
        ],
        "highest_qualification",
    ] = pd.NA

    delivery.loc[
        defect_indices[
            "unknown_code"
        ],
        "highest_qualification",
    ] = "UNKNOWN_CODE"

    delivery.loc[
        defect_indices[
            "invalid_year"
        ],
        "reference_year",
    ] = invalid_reference_year

    delivery.loc[
        defect_indices[
            "missing_person_id"
        ],
        "person_id",
    ] = pd.NA

    duplicates = (
        delivery.iloc[
            defect_indices[
                "duplicate_records"
            ]
        ]
        .copy()
    )

    delivery = pd.concat(
        [
            delivery,
            duplicates,
        ],
        ignore_index=True,
    )

    expected_rows = (
        n_base
        + defect_counts[
            "duplicate_records"
        ]
    )

    if len(delivery) != expected_rows:
        raise RuntimeError(
            "Unexpected number of Zensus-like delivery rows."
        )

    if int(
        delivery[
            "person_id"
        ].isna().sum()
    ) != defect_counts[
        "missing_person_id"
    ]:
        raise RuntimeError(
            "Unexpected number of missing Zensus person IDs."
        )

    if int(
        delivery[
            "highest_qualification"
        ].isna().sum()
    ) != defect_counts[
        "missing_qualification"
    ]:
        raise RuntimeError(
            "Unexpected number of missing Zensus qualifications."
        )

    if int(
        delivery[
            "highest_qualification"
        ].eq(
            "UNKNOWN_CODE"
        ).sum()
    ) != defect_counts[
        "unknown_code"
    ]:
        raise RuntimeError(
            "Unexpected number of unknown Zensus codes."
        )

    if int(
        delivery[
            "reference_year"
        ].ne(
            reference_year
        ).sum()
    ) != defect_counts[
        "invalid_year"
    ]:
        raise RuntimeError(
            "Unexpected number of invalid Zensus years."
        )

    duplicate_key_counts = (
        delivery.loc[
            delivery[
                "person_id"
            ].notna()
        ]
        .groupby(
            [
                "person_id",
                "reference_year",
            ]
        )
        .size()
    )

    n_duplicate_keys = int(
        (
            duplicate_key_counts > 1
        ).sum()
    )

    if (
        n_duplicate_keys
        != defect_counts[
            "duplicate_records"
        ]
    ):
        raise RuntimeError(
            "Unexpected number of duplicated "
            "Zensus person-year keys."
        )

    valid_person_ids = set(
        delivery[
            "person_id"
        ].dropna()
    )

    truth_person_ids = set(
        truth_2022[
            "person_id"
        ]
    )

    if not valid_person_ids.issubset(
        truth_person_ids
    ):
        raise RuntimeError(
            "Zensus delivery contains persons "
            "outside 2022 education truth."
        )

    return delivery[
        ZENSUS_2022_COLUMNS
    ]
