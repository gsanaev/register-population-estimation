"""Generate imperfect population and administrative activity sources."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd

from simulation.world import age_group_from_age


REGISTRATION_STATUS_CATEGORIES = (
    "main_residence",
    "secondary_residence",
)

POPULATION_REGISTER_COLUMNS = [
    "person_id",
    "household_id",
    "address_id",
    "region_code",
    "municipality_code",
    "sex",
    "age",
    "age_group",
    "citizenship_group",
    "registration_status",
    "registration_date",
    "last_move_date",
]

POPULATION_TRUTH_REQUIRED_COLUMNS = {
    "person_id",
    "true_resident",
    "sex",
    "age",
    "age_group",
    "citizenship_group",
    "true_household_id",
    "true_address_id",
    "true_region_code",
    "true_municipality_code",
    "former_household_id",
    "former_address_id",
    "former_region_code",
    "former_municipality_code",
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


def _validated_probability_vector(
    probability_config: Mapping[str, Any],
    categories: tuple[str, ...],
    name: str,
) -> np.ndarray:
    """Validate named probabilities in a stable category order."""

    if not isinstance(probability_config, Mapping):
        raise ValueError(
            f"{name} must be a mapping of categories to probabilities."
        )

    if set(probability_config) != set(categories):
        raise ValueError(
            f"{name} must contain exactly: "
            + ", ".join(categories)
        )

    probabilities = np.array(
        [
            probability_config[category]
            for category in categories
        ],
        dtype=float,
    )

    if np.any(probabilities < 0):
        raise ValueError(
            f"{name} probabilities must be non-negative."
        )

    if not np.isclose(probabilities.sum(), 1.0):
        raise ValueError(
            f"{name} probabilities must sum to 1."
        )

    return probabilities


def _sample_uniform_dates(
    start_value: Any,
    end_value: Any,
    size: int,
    rng: np.random.Generator,
) -> pd.Series:
    """Sample dates uniformly over an inclusive calendar interval."""

    if start_value is None or end_value is None:
        raise ValueError(
            "Date-range endpoints must be configured."
        )

    start = pd.Timestamp(start_value)
    end = pd.Timestamp(end_value)

    if end < start:
        raise ValueError(
            "Date-range end must not precede its start."
        )

    n_days = (end - start).days + 1

    offsets = rng.integers(
        0,
        n_days,
        size=size,
    )

    return pd.Series(
        start
        + pd.to_timedelta(
            offsets,
            unit="D",
        ),
        dtype="datetime64[ns]",
    )


def generate_population_register(
    population_truth: pd.DataFrame,
    age_bands: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate an imperfect population-register delivery."""

    missing_truth_columns = (
        POPULATION_TRUTH_REQUIRED_COLUMNS
        - set(population_truth.columns)
    )

    if missing_truth_columns:
        raise ValueError(
            "Population truth is missing required fields: "
            + ", ".join(
                sorted(missing_truth_columns)
            )
        )

    if population_truth["person_id"].duplicated().any():
        raise ValueError(
            "Population truth contains duplicated person IDs."
        )

    residence_states = set(
        population_truth[
            "true_resident"
        ].dropna().unique()
    )

    if residence_states != {0, 1}:
        raise ValueError(
            "Population truth must contain true_resident states 0 and 1."
        )

    register_config = config.get(
        "population_register"
    )

    if not isinstance(register_config, Mapping):
        raise ValueError(
            "Configuration must contain a population_register section."
        )

    undercoverage_probability = _validate_probability(
        register_config.get(
            "undercoverage_probability"
        ),
        "undercoverage_probability",
    )

    stale_former_probability = _validate_probability(
        register_config.get(
            "stale_former_probability"
        ),
        "stale_former_probability",
    )

    status_probabilities = (
        _validated_probability_vector(
            register_config.get(
                "registration_status_probabilities"
            ),
            REGISTRATION_STATUS_CATEGORIES,
            "registration_status_probabilities",
        )
    )

    imperfections = register_config.get(
        "imperfections"
    )

    if not isinstance(imperfections, Mapping):
        raise ValueError(
            "population_register must define imperfections."
        )

    citizenship_missing_probability = (
        _validate_probability(
            imperfections.get(
                "citizenship_missing_probability"
            ),
            "citizenship_missing_probability",
        )
    )

    status_missing_probability = (
        _validate_probability(
            imperfections.get(
                "registration_status_missing_probability"
            ),
            "registration_status_missing_probability",
        )
    )

    last_move_shift_probability = (
        _validate_probability(
            imperfections.get(
                "last_move_shift_probability"
            ),
            "last_move_shift_probability",
        )
    )

    age_outlier_probability = (
        _validate_probability(
            imperfections.get(
                "age_outlier_probability"
            ),
            "age_outlier_probability",
        )
    )

    shift_min = imperfections.get(
        "last_move_shift_days_min"
    )
    shift_max = imperfections.get(
        "last_move_shift_days_max"
    )
    outlier_min = imperfections.get(
        "age_outlier_min"
    )
    outlier_max = imperfections.get(
        "age_outlier_max"
    )

    if (
        not isinstance(shift_min, int)
        or not isinstance(shift_max, int)
        or shift_min < 1
        or shift_max < shift_min
    ):
        raise ValueError(
            "Last-move shift limits are invalid."
        )

    if (
        not isinstance(outlier_min, int)
        or not isinstance(outlier_max, int)
        or outlier_min < 0
        or outlier_max < outlier_min
    ):
        raise ValueError(
            "Age-outlier limits are invalid."
        )

    coverage_draw = rng.random(
        len(population_truth)
    )

    true_resident = (
        population_truth[
            "true_resident"
        ].to_numpy(dtype=np.int64)
    )

    included = np.where(
        true_resident == 1,
        coverage_draw >= undercoverage_probability,
        coverage_draw < stale_former_probability,
    )

    observed = (
        population_truth.loc[
            included
        ]
        .reset_index(drop=True)
        .copy()
    )

    is_current = (
        observed["true_resident"]
        .to_numpy(dtype=np.int64)
        == 1
    )

    population_register = pd.DataFrame(
        {
            "person_id":
                observed["person_id"].copy(),
            "sex":
                observed["sex"].copy(),
            "age":
                observed["age"].to_numpy(
                    dtype=np.int64
                ),
            "age_group":
                observed["age_group"].copy(),
            "citizenship_group":
                observed[
                    "citizenship_group"
                ].copy(),
        }
    )

    location_pairs = (
        (
            "household_id",
            "true_household_id",
            "former_household_id",
        ),
        (
            "address_id",
            "true_address_id",
            "former_address_id",
        ),
        (
            "region_code",
            "true_region_code",
            "former_region_code",
        ),
        (
            "municipality_code",
            "true_municipality_code",
            "former_municipality_code",
        ),
    )

    for (
        output_column,
        current_column,
        former_column,
    ) in location_pairs:
        values = observed[
            former_column
        ].copy()

        values.loc[is_current] = (
            observed.loc[
                is_current,
                current_column,
            ]
        )

        population_register[
            output_column
        ] = values.to_numpy(dtype=object)

    n_records = len(population_register)

    population_register[
        "registration_status"
    ] = rng.choice(
        REGISTRATION_STATUS_CATEGORIES,
        size=n_records,
        replace=True,
        p=status_probabilities,
    )

    registration_dates = _sample_uniform_dates(
        register_config.get(
            "registration_date_start"
        ),
        register_config.get(
            "registration_date_end"
        ),
        n_records,
        rng,
    )

    last_move_dates = _sample_uniform_dates(
        register_config.get(
            "last_move_date_start"
        ),
        register_config.get(
            "last_move_date_end"
        ),
        n_records,
        rng,
    )

    last_move_dates = pd.Series(
        np.maximum(
            last_move_dates.to_numpy(
                dtype="datetime64[ns]"
            ),
            registration_dates.to_numpy(
                dtype="datetime64[ns]"
            ),
        ),
        dtype="datetime64[ns]",
    )

    population_register[
        "registration_date"
    ] = registration_dates

    population_register[
        "last_move_date"
    ] = last_move_dates

    citizenship_missing = (
        rng.random(n_records)
        < citizenship_missing_probability
    )

    population_register.loc[
        citizenship_missing,
        "citizenship_group",
    ] = pd.NA

    status_missing = (
        rng.random(n_records)
        < status_missing_probability
    )

    population_register.loc[
        status_missing,
        "registration_status",
    ] = pd.NA

    move_shifted = (
        rng.random(n_records)
        < last_move_shift_probability
    )

    n_shifted = int(
        move_shifted.sum()
    )

    if n_shifted > 0:
        shift_days = rng.integers(
            shift_min,
            shift_max + 1,
            size=n_shifted,
        )

        population_register.loc[
            move_shifted,
            "last_move_date",
        ] = (
            population_register.loc[
                move_shifted,
                "last_move_date",
            ].to_numpy(
                dtype="datetime64[ns]"
            )
            + shift_days.astype(
                "timedelta64[D]"
            )
        )

    age_outlier = (
        rng.random(n_records)
        < age_outlier_probability
    )

    n_age_outliers = int(
        age_outlier.sum()
    )

    if n_age_outliers > 0:
        outlier_ages = rng.integers(
            outlier_min,
            outlier_max + 1,
            size=n_age_outliers,
        )

        population_register.loc[
            age_outlier,
            "age",
        ] = outlier_ages

        population_register.loc[
            age_outlier,
            "age_group",
        ] = age_group_from_age(
            outlier_ages,
            age_bands,
        )

    if population_register[
        "person_id"
    ].duplicated().any():
        raise RuntimeError(
            "Population register contains duplicated person IDs."
        )

    if population_register[
        [
            "household_id",
            "address_id",
            "region_code",
            "municipality_code",
        ]
    ].isna().any().any():
        raise RuntimeError(
            "Population-register geography is incomplete."
        )

    return population_register[
        POPULATION_REGISTER_COLUMNS
    ]
