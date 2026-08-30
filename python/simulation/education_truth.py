"""Generate longitudinal synthetic educational-attainment truth."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd


ATTAINMENT_LABELS = (
    "low_or_none",
    "school_qualification",
    "vocational_or_postsecondary",
    "bachelor_or_equivalent",
    "master_or_equivalent",
    "doctorate",
)

EDUCATION_TRUTH_COLUMNS = [
    "person_id",
    "reference_year",
    "age_at_reference_year",
    "true_attainment_level",
    "true_attainment_label",
]

POPULATION_REQUIRED_COLUMNS = {
    "person_id",
    "true_resident",
    "age",
}

BASE_PROBABILITY_PROFILES = (
    "age_15_17",
    "age_18_24",
    "age_25_39",
    "age_40_64",
    "age_65_plus",
)

PROGRESSION_PROFILES = (
    "age_15_17",
    "age_18_24",
    "age_25_30",
    "age_31_39",
    "other",
)


def _get_truth_config(
    config: Mapping[str, Any],
) -> Mapping[str, Any]:
    """Return and validate the education-truth configuration."""

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

    return truth_config


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


def _validate_probability_vector(
    values: Any,
    name: str,
) -> np.ndarray:
    """Validate a six-level attainment probability vector."""

    probabilities = np.asarray(
        values,
        dtype=float,
    )

    if probabilities.shape != (
        len(ATTAINMENT_LABELS),
    ):
        raise ValueError(
            f"{name} must contain exactly "
            f"{len(ATTAINMENT_LABELS)} probabilities."
        )

    if np.any(probabilities < 0):
        raise ValueError(
            f"{name} probabilities must be non-negative."
        )

    if not np.isclose(
        probabilities.sum(),
        1.0,
    ):
        raise ValueError(
            f"{name} probabilities must sum to 1."
        )

    return probabilities


def _minimum_age_by_level(
    config: Mapping[str, Any],
) -> np.ndarray:
    """Return validated minimum ages for attainment levels 1-6."""

    truth_config = _get_truth_config(
        config
    )

    values = truth_config.get(
        "minimum_age_by_level"
    )

    minimum_ages = np.asarray(
        values,
        dtype=np.int64,
    )

    if minimum_ages.shape != (
        len(ATTAINMENT_LABELS),
    ):
        raise ValueError(
            "minimum_age_by_level must contain "
            "exactly six values."
        )

    if np.any(minimum_ages < 0):
        raise ValueError(
            "minimum_age_by_level must be non-negative."
        )

    if np.any(
        np.diff(minimum_ages) < 0
    ):
        raise ValueError(
            "minimum_age_by_level must be non-decreasing."
        )

    return minimum_ages


def attainment_label(
    levels: np.ndarray,
) -> np.ndarray:
    """Map attainment levels 1-6 to their synthetic labels."""

    level_values = np.asarray(
        levels,
        dtype=np.int64,
    )

    if (
        level_values.ndim != 1
        or np.any(level_values < 1)
        or np.any(
            level_values
            > len(ATTAINMENT_LABELS)
        )
    ):
        raise ValueError(
            "Attainment levels must be a one-dimensional "
            "array containing only levels 1-6."
        )

    labels = np.asarray(
        ATTAINMENT_LABELS,
        dtype=object,
    )

    return labels[
        level_values - 1
    ]


def _base_probability_matrix(
    ages: np.ndarray,
    config: Mapping[str, Any],
) -> np.ndarray:
    """Build base attainment probabilities for an age vector."""

    truth_config = _get_truth_config(
        config
    )

    probability_config = (
        truth_config.get(
            "base_attainment_probabilities"
        )
    )

    if not isinstance(
        probability_config,
        Mapping,
    ):
        raise ValueError(
            "education.truth must define "
            "base_attainment_probabilities."
        )

    probabilities = {
        profile: _validate_probability_vector(
            probability_config.get(
                profile
            ),
            profile,
        )
        for profile in BASE_PROBABILITY_PROFILES
    }

    age_values = np.asarray(
        ages,
        dtype=np.int64,
    )

    if age_values.ndim != 1:
        raise ValueError(
            "Ages must be one-dimensional."
        )

    matrix = np.empty(
        (
            len(age_values),
            len(ATTAINMENT_LABELS),
        ),
        dtype=float,
    )

    age_15_17 = age_values <= 17
    age_18_24 = (
        (age_values >= 18)
        & (age_values <= 24)
    )
    age_25_39 = (
        (age_values >= 25)
        & (age_values <= 39)
    )
    age_40_64 = (
        (age_values >= 40)
        & (age_values <= 64)
    )
    age_65_plus = age_values >= 65

    matrix[
        age_15_17
    ] = probabilities[
        "age_15_17"
    ]

    matrix[
        age_18_24
    ] = probabilities[
        "age_18_24"
    ]

    matrix[
        age_25_39
    ] = probabilities[
        "age_25_39"
    ]

    matrix[
        age_40_64
    ] = probabilities[
        "age_40_64"
    ]

    matrix[
        age_65_plus
    ] = probabilities[
        "age_65_plus"
    ]

    return matrix


def base_attainment_probabilities(
    age_value: int,
    config: Mapping[str, Any],
) -> np.ndarray:
    """Return the configured base probabilities for one age."""

    return _base_probability_matrix(
        np.array(
            [age_value],
            dtype=np.int64,
        ),
        config,
    )[0]


def sample_attainment_by_age(
    age_values: np.ndarray,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> np.ndarray:
    """Sample age-plausible attainment levels for ages 15 and older."""

    ages = np.asarray(
        age_values,
        dtype=np.int64,
    )

    if ages.ndim != 1:
        raise ValueError(
            "Attainment generation requires one-dimensional ages."
        )

    if (
        len(ages) > 0
        and np.any(ages < 15)
    ):
        raise ValueError(
            "Attainment generation requires ages of 15 or older."
        )

    if len(ages) == 0:
        return np.empty(
            0,
            dtype=np.int64,
        )

    minimum_ages = (
        _minimum_age_by_level(
            config
        )
    )

    probabilities = (
        _base_probability_matrix(
            ages,
            config,
        )
    )

    allowed = (
        ages[:, None]
        >= minimum_ages[None, :]
    )

    probabilities = (
        probabilities
        * allowed
    )

    row_totals = probabilities.sum(
        axis=1
    )

    if np.any(row_totals <= 0):
        raise ValueError(
            "At least one age has no allowed attainment level."
        )

    probabilities = (
        probabilities
        / row_totals[:, None]
    )

    cumulative = np.cumsum(
        probabilities,
        axis=1,
    )

    cumulative[
        :,
        -1,
    ] = 1.0

    draws = rng.random(
        len(ages)
    )

    levels = (
        (
            draws[:, None]
            > cumulative
        ).sum(axis=1)
        + 1
    )

    return levels.astype(
        np.int64
    )


def progress_attainment(
    current_level: np.ndarray,
    age_at_start: np.ndarray,
    age_at_end: np.ndarray,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> np.ndarray:
    """Apply at most one age-plausible upward attainment progression."""

    levels = np.asarray(
        current_level,
        dtype=np.int64,
    )

    start_ages = np.asarray(
        age_at_start,
        dtype=np.int64,
    )

    end_ages = np.asarray(
        age_at_end,
        dtype=np.int64,
    )

    if not (
        levels.ndim
        == start_ages.ndim
        == end_ages.ndim
        == 1
    ):
        raise ValueError(
            "Progression inputs must be one-dimensional."
        )

    if not (
        len(levels)
        == len(start_ages)
        == len(end_ages)
    ):
        raise ValueError(
            "Progression inputs must have equal lengths."
        )

    if (
        np.any(levels < 1)
        or np.any(
            levels
            > len(ATTAINMENT_LABELS)
        )
    ):
        raise ValueError(
            "Current attainment levels must be between 1 and 6."
        )

    if np.any(start_ages < 15):
        raise ValueError(
            "Progression requires starting ages of 15 or older."
        )

    if np.any(end_ages < start_ages):
        raise ValueError(
            "Progression end ages must not precede start ages."
        )

    truth_config = _get_truth_config(
        config
    )

    progression_config = (
        truth_config.get(
            "progression_probabilities"
        )
    )

    if not isinstance(
        progression_config,
        Mapping,
    ):
        raise ValueError(
            "education.truth must define "
            "progression_probabilities."
        )

    progression_probabilities = {
        profile: _validate_probability(
            progression_config.get(
                profile
            ),
            profile,
        )
        for profile in PROGRESSION_PROFILES
    }

    progression_probability = np.full(
        len(levels),
        progression_probabilities[
            "other"
        ],
        dtype=float,
    )

    progression_probability[
        (start_ages >= 15)
        & (start_ages <= 17)
    ] = progression_probabilities[
        "age_15_17"
    ]

    progression_probability[
        (start_ages >= 18)
        & (start_ages <= 24)
    ] = progression_probabilities[
        "age_18_24"
    ]

    progression_probability[
        (start_ages >= 25)
        & (start_ages <= 30)
    ] = progression_probabilities[
        "age_25_30"
    ]

    progression_probability[
        (start_ages >= 31)
        & (start_ages <= 39)
    ] = progression_probabilities[
        "age_31_39"
    ]

    next_level = np.minimum(
        levels + 1,
        len(ATTAINMENT_LABELS),
    )

    minimum_ages = (
        _minimum_age_by_level(
            config
        )
    )

    can_progress = (
        (levels < len(ATTAINMENT_LABELS))
        & (
            end_ages
            >= minimum_ages[
                next_level - 1
            ]
        )
    )

    progression_draw = rng.random(
        len(levels)
    )

    progressed = np.where(
        can_progress
        & (
            progression_draw
            < progression_probability
        ),
        next_level,
        levels,
    )

    return progressed.astype(
        np.int64
    )


def _validate_population_truth(
    population_truth: pd.DataFrame,
) -> None:
    """Validate the population fields required for education truth."""

    missing_columns = (
        POPULATION_REQUIRED_COLUMNS
        - set(population_truth.columns)
    )

    if missing_columns:
        raise ValueError(
            "Population truth is missing education-truth fields: "
            + ", ".join(
                sorted(missing_columns)
            )
        )

    if population_truth[
        "person_id"
    ].isna().any():
        raise ValueError(
            "Population truth contains missing person IDs."
        )

    if population_truth[
        "person_id"
    ].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    if population_truth[
        "true_resident"
    ].isna().any():
        raise ValueError(
            "Population truth contains missing true_resident values."
        )

    residence_states = set(
        population_truth[
            "true_resident"
        ].unique()
    )

    if not residence_states.issubset(
        {
            0,
            1,
        }
    ):
        raise ValueError(
            "true_resident must contain only 0 and 1."
        )

    if population_truth[
        "age"
    ].isna().any():
        raise ValueError(
            "Population truth contains missing ages."
        )


def _validate_generated_truth(
    education_truth: pd.DataFrame,
    config: Mapping[str, Any],
) -> None:
    """Validate the generated longitudinal education truth."""

    if list(
        education_truth.columns
    ) != EDUCATION_TRUTH_COLUMNS:
        raise RuntimeError(
            "Education truth has an unexpected schema."
        )

    if education_truth.duplicated(
        subset=[
            "person_id",
            "reference_year",
        ]
    ).any():
        raise RuntimeError(
            "Education truth contains duplicated person-year records."
        )

    truth_config = _get_truth_config(
        config
    )

    minimum_truth_age = int(
        truth_config.get(
            "minimum_truth_age"
        )
    )

    if (
        education_truth[
            "age_at_reference_year"
        ].lt(
            minimum_truth_age
        ).any()
    ):
        raise RuntimeError(
            "Education truth contains persons "
            "below the minimum truth age."
        )

    levels = education_truth[
        "true_attainment_level"
    ].to_numpy(
        dtype=np.int64
    )

    if (
        np.any(levels < 1)
        or np.any(
            levels
            > len(ATTAINMENT_LABELS)
        )
    ):
        raise RuntimeError(
            "Education truth contains invalid attainment levels."
        )

    minimum_ages = (
        _minimum_age_by_level(
            config
        )
    )

    observed_ages = education_truth[
        "age_at_reference_year"
    ].to_numpy(
        dtype=np.int64
    )

    if np.any(
        observed_ages
        < minimum_ages[
            levels - 1
        ]
    ):
        raise RuntimeError(
            "Education truth contains age-attainment "
            "combinations below the plausibility thresholds."
        )

    expected_labels = (
        attainment_label(
            levels
        )
    )

    if not np.array_equal(
        education_truth[
            "true_attainment_label"
        ].to_numpy(dtype=object),
        expected_labels,
    ):
        raise RuntimeError(
            "Education truth contains inconsistent attainment labels."
        )


def generate_education_truth(
    population_truth: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate longitudinal hidden attainment truth for 2022 and 2024."""

    _validate_population_truth(
        population_truth
    )

    truth_config = _get_truth_config(
        config
    )

    reference_year_2022 = (
        truth_config.get(
            "reference_year_2022"
        )
    )
    reference_year_2024 = (
        truth_config.get(
            "reference_year_2024"
        )
    )
    age_offset_2022 = (
        truth_config.get(
            "age_offset_2022"
        )
    )
    age_offset_2024 = (
        truth_config.get(
            "age_offset_2024"
        )
    )
    minimum_truth_age = (
        truth_config.get(
            "minimum_truth_age"
        )
    )

    integer_values = {
        "reference_year_2022":
            reference_year_2022,
        "reference_year_2024":
            reference_year_2024,
        "age_offset_2022":
            age_offset_2022,
        "age_offset_2024":
            age_offset_2024,
        "minimum_truth_age":
            minimum_truth_age,
    }

    if any(
        not isinstance(value, int)
        for value in integer_values.values()
    ):
        raise ValueError(
            "Education truth years, age offsets, "
            "and minimum age must be integers."
        )

    if (
        reference_year_2024
        <= reference_year_2022
    ):
        raise ValueError(
            "The 2024 education reference year "
            "must follow the 2022 reference year."
        )

    if (
        age_offset_2022 < 0
        or age_offset_2024 < 0
    ):
        raise ValueError(
            "Education age offsets must be non-negative."
        )

    residents = (
        population_truth.loc[
            population_truth[
                "true_resident"
            ].eq(1),
            [
                "person_id",
                "age",
            ],
        ]
        .reset_index(drop=True)
        .copy()
    )

    residents[
        "age_2022"
    ] = (
        residents["age"]
        - age_offset_2022
    )

    residents[
        "age_2024"
    ] = (
        residents["age"]
        - age_offset_2024
    )

    eligible_2022 = (
        residents[
            "age_2022"
        ]
        >= minimum_truth_age
    )

    population_2022 = (
        residents.loc[
            eligible_2022
        ]
        .reset_index(drop=True)
        .copy()
    )

    levels_2022 = (
        sample_attainment_by_age(
            population_2022[
                "age_2022"
            ].to_numpy(
                dtype=np.int64
            ),
            config,
            rng,
        )
    )

    education_truth_2022 = pd.DataFrame(
        {
            "person_id":
                population_2022[
                    "person_id"
                ].to_numpy(dtype=object),
            "reference_year":
                np.full(
                    len(population_2022),
                    reference_year_2022,
                    dtype=np.int64,
                ),
            "age_at_reference_year":
                population_2022[
                    "age_2022"
                ].to_numpy(
                    dtype=np.int64
                ),
            "true_attainment_level":
                levels_2022,
            "true_attainment_label":
                attainment_label(
                    levels_2022
                ),
        }
    )

    age_2024_existing = (
        population_2022[
            "age_2024"
        ].to_numpy(
            dtype=np.int64
        )
    )

    levels_2024_existing = (
        progress_attainment(
            levels_2022,
            population_2022[
                "age_2022"
            ].to_numpy(
                dtype=np.int64
            ),
            age_2024_existing,
            config,
            rng,
        )
    )

    education_truth_2024_existing = (
        pd.DataFrame(
            {
                "person_id":
                    population_2022[
                        "person_id"
                    ].to_numpy(
                        dtype=object
                    ),
                "reference_year":
                    np.full(
                        len(population_2022),
                        reference_year_2024,
                        dtype=np.int64,
                    ),
                "age_at_reference_year":
                    age_2024_existing,
                "true_attainment_level":
                    levels_2024_existing,
                "true_attainment_label":
                    attainment_label(
                        levels_2024_existing
                    ),
            }
        )
    )

    newly_eligible = (
        (~eligible_2022)
        & (
            residents[
                "age_2024"
            ]
            >= minimum_truth_age
        )
    )

    population_2024_new = (
        residents.loc[
            newly_eligible
        ]
        .reset_index(drop=True)
        .copy()
    )

    levels_2024_new = (
        sample_attainment_by_age(
            population_2024_new[
                "age_2024"
            ].to_numpy(
                dtype=np.int64
            ),
            config,
            rng,
        )
    )

    education_truth_2024_new = (
        pd.DataFrame(
            {
                "person_id":
                    population_2024_new[
                        "person_id"
                    ].to_numpy(
                        dtype=object
                    ),
                "reference_year":
                    np.full(
                        len(population_2024_new),
                        reference_year_2024,
                        dtype=np.int64,
                    ),
                "age_at_reference_year":
                    population_2024_new[
                        "age_2024"
                    ].to_numpy(
                        dtype=np.int64
                    ),
                "true_attainment_level":
                    levels_2024_new,
                "true_attainment_label":
                    attainment_label(
                        levels_2024_new
                    ),
            }
        )
    )

    education_truth = pd.concat(
        [
            education_truth_2022,
            education_truth_2024_existing,
            education_truth_2024_new,
        ],
        ignore_index=True,
    )

    education_truth = (
        education_truth.sort_values(
            [
                "person_id",
                "reference_year",
            ],
            kind="mergesort",
        )
        .reset_index(drop=True)
    )

    education_truth = education_truth[
        EDUCATION_TRUTH_COLUMNS
    ]

    _validate_generated_truth(
        education_truth,
        config,
    )

    progression_check = (
        education_truth_2022[
            [
                "person_id",
                "true_attainment_level",
            ]
        ]
        .rename(
            columns={
                "true_attainment_level":
                    "level_2022",
            }
        )
        .merge(
            education_truth_2024_existing[
                [
                    "person_id",
                    "true_attainment_level",
                ]
            ].rename(
                columns={
                    "true_attainment_level":
                        "level_2024",
                }
            ),
            on="person_id",
            how="inner",
            validate="one_to_one",
        )
    )

    if (
        progression_check[
            "level_2024"
        ]
        .lt(
            progression_check[
                "level_2022"
            ]
        )
        .any()
    ):
        raise RuntimeError(
            "Education truth contains downward attainment progression."
        )

    return education_truth
