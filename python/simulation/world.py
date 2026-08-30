"""Generate the hidden synthetic population world."""

from collections.abc import Mapping
from typing import Any

import numpy as np
import pandas as pd


REGION_COLUMNS = [
    "region_code",
    "region_name",
    "urbanicity",
    "population_weight",
]

AGE_BAND_COLUMNS = [
    "age_band",
    "min_age",
    "max_sample_age",
    "resident_probability",
    "former_probability",
]

VALID_URBANICITY = {
    "urban",
    "mixed",
    "rural",
}


def build_regions(config: Mapping[str, Any]) -> pd.DataFrame:
    """Build and validate the synthetic regional reference table."""

    region_rows = config.get("regions")

    if not isinstance(region_rows, list) or not region_rows:
        raise ValueError("Configuration must contain a non-empty regions list.")

    regions = pd.DataFrame(region_rows)

    missing_columns = set(REGION_COLUMNS) - set(regions.columns)

    if missing_columns:
        raise ValueError(
            "Region configuration is missing required fields: "
            + ", ".join(sorted(missing_columns))
        )

    regions = regions[REGION_COLUMNS].copy()

    if regions["region_code"].isna().any():
        raise ValueError("Region codes must not be missing.")

    if regions["region_code"].duplicated().any():
        raise ValueError("Region codes must be unique.")

    if not set(regions["urbanicity"]).issubset(VALID_URBANICITY):
        raise ValueError("Region configuration contains invalid urbanicity values.")

    weights = regions["population_weight"].to_numpy(dtype=float)

    if np.any(weights < 0):
        raise ValueError("Region population weights must be non-negative.")

    if not np.isclose(weights.sum(), 1.0):
        raise ValueError("Region population weights must sum to 1.")

    return regions


def build_age_bands(config: Mapping[str, Any]) -> pd.DataFrame:
    """Build and validate the synthetic age-band reference table."""

    demographics = config.get("demographics")

    if not isinstance(demographics, Mapping):
        raise ValueError("Configuration must contain a demographics section.")

    age_band_rows = demographics.get("age_bands")

    if not isinstance(age_band_rows, list) or not age_band_rows:
        raise ValueError(
            "Demographics configuration must contain a non-empty age_bands list."
        )

    age_bands = pd.DataFrame(age_band_rows)

    missing_columns = set(AGE_BAND_COLUMNS) - set(age_bands.columns)

    if missing_columns:
        raise ValueError(
            "Age-band configuration is missing required fields: "
            + ", ".join(sorted(missing_columns))
        )

    age_bands = age_bands[AGE_BAND_COLUMNS].copy()

    if age_bands["age_band"].duplicated().any():
        raise ValueError("Age-band labels must be unique.")

    min_ages = age_bands["min_age"].to_numpy(dtype=int)
    max_sample_ages = age_bands["max_sample_age"].to_numpy(dtype=int)

    if np.any(min_ages < 0):
        raise ValueError("Minimum ages must be non-negative.")

    if np.any(max_sample_ages < min_ages):
        raise ValueError("Maximum sampling ages must not be below minimum ages.")

    if np.any(np.diff(min_ages) <= 0):
        raise ValueError("Age bands must be ordered by strictly increasing minimum age.")

    for probability_column in (
        "resident_probability",
        "former_probability",
    ):
        probabilities = age_bands[probability_column].to_numpy(dtype=float)

        if np.any(probabilities < 0):
            raise ValueError(
                f"{probability_column} values must be non-negative."
            )

        if not np.isclose(probabilities.sum(), 1.0):
            raise ValueError(
                f"{probability_column} values must sum to 1."
            )

    return age_bands


def sample_age(
    n: int,
    age_bands: pd.DataFrame,
    probability_column: str,
    rng: np.random.Generator,
) -> np.ndarray:
    """Sample integer ages via an age-band distribution."""

    if n < 0:
        raise ValueError("Sample size must be non-negative.")

    if probability_column not in age_bands.columns:
        raise KeyError(
            f"Unknown age-band probability column: {probability_column}"
        )

    probabilities = age_bands[probability_column].to_numpy(dtype=float)

    if np.any(probabilities < 0) or not np.isclose(
        probabilities.sum(),
        1.0,
    ):
        raise ValueError(
            f"{probability_column} must contain non-negative probabilities "
            "that sum to 1."
        )

    sampled_band_positions = rng.choice(
        len(age_bands),
        size=n,
        replace=True,
        p=probabilities,
    )

    ages = np.empty(n, dtype=np.int64)

    for position in range(len(age_bands)):
        selected = sampled_band_positions == position
        n_selected = int(selected.sum())

        if n_selected == 0:
            continue

        lower = int(age_bands.iloc[position]["min_age"])
        upper = int(age_bands.iloc[position]["max_sample_age"])

        ages[selected] = rng.integers(
            lower,
            upper + 1,
            size=n_selected,
        )

    return ages


def age_group_from_age(
    ages: np.ndarray,
    age_bands: pd.DataFrame,
) -> np.ndarray:
    """Assign configured age-group labels from non-negative integer ages."""

    age_values = np.asarray(ages, dtype=np.int64)

    if age_values.ndim != 1:
        raise ValueError("Ages must be a one-dimensional array.")

    if np.any(age_values < 0):
        raise ValueError("Ages must be non-negative.")

    minimum_ages = age_bands["min_age"].to_numpy(dtype=np.int64)
    labels = age_bands["age_band"].to_numpy(dtype=object)

    band_positions = np.searchsorted(
        minimum_ages,
        age_values,
        side="right",
    ) - 1

    if np.any(band_positions < 0):
        raise ValueError("Unable to assign an age group.")

    return labels[band_positions]
