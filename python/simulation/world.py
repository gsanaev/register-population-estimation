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

VALID_URBANICITY = (
    "urban",
    "mixed",
    "rural",
)

ADDRESS_TYPES = (
    "single_family",
    "multi_family",
    "large_residential",
)

ADDRESS_COLUMNS = [
    "address_id",
    "region_code",
    "region_name",
    "municipality_code",
    "urbanicity",
    "address_type",
]


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


def generate_address_register(
    regions: pd.DataFrame,
    config: Mapping[str, Any],
    rng: np.random.Generator,
) -> pd.DataFrame:
    """Generate the synthetic address reference register."""

    address_config = config.get("addresses")

    if not isinstance(address_config, Mapping):
        raise ValueError("Configuration must contain an addresses section.")

    n_addresses = address_config.get("n_addresses")
    municipalities_per_region = address_config.get(
        "municipalities_per_region"
    )
    type_probabilities = address_config.get(
        "address_type_probabilities"
    )

    if not isinstance(n_addresses, int) or n_addresses <= 0:
        raise ValueError("n_addresses must be a positive integer.")

    if (
        not isinstance(municipalities_per_region, int)
        or municipalities_per_region <= 0
    ):
        raise ValueError(
            "municipalities_per_region must be a positive integer."
        )

    if not isinstance(type_probabilities, Mapping):
        raise ValueError(
            "addresses must define address_type_probabilities."
        )

    for urbanicity in VALID_URBANICITY:
        probabilities_by_type = type_probabilities.get(
            urbanicity
        )

        if not isinstance(probabilities_by_type, Mapping):
            raise ValueError(
                "Missing address-type probabilities for "
                f"{urbanicity}."
            )

        if set(probabilities_by_type) != set(ADDRESS_TYPES):
            raise ValueError(
                "Address-type probability definitions must contain "
                "exactly the configured address types."
            )

        probabilities = np.array(
            [
                probabilities_by_type[address_type]
                for address_type in ADDRESS_TYPES
            ],
            dtype=float,
        )

        if np.any(probabilities < 0):
            raise ValueError(
                "Address-type probabilities must be non-negative."
            )

        if not np.isclose(probabilities.sum(), 1.0):
            raise ValueError(
                "Address-type probabilities must sum to 1."
            )

    region_codes = regions["region_code"].to_numpy(
        dtype=object
    )
    region_weights = regions["population_weight"].to_numpy(
        dtype=float
    )

    sampled_region_codes = rng.choice(
        region_codes,
        size=n_addresses,
        replace=True,
        p=region_weights,
    )

    address_register = pd.DataFrame(
        {
            "address_id": [
                f"A{number:06d}"
                for number in range(
                    1,
                    n_addresses + 1,
                )
            ],
            "region_code": sampled_region_codes,
        }
    )

    address_register = address_register.merge(
        regions[
            [
                "region_code",
                "region_name",
                "urbanicity",
            ]
        ],
        on="region_code",
        how="left",
        validate="many_to_one",
        sort=False,
    )

    municipality_numbers = rng.integers(
        1,
        municipalities_per_region + 1,
        size=n_addresses,
    )

    address_register["municipality_code"] = [
        f"{region_code}-M{municipality_number:02d}"
        for region_code, municipality_number in zip(
            address_register["region_code"],
            municipality_numbers,
            strict=True,
        )
    ]

    address_types = np.empty(
        n_addresses,
        dtype=object,
    )

    for urbanicity in VALID_URBANICITY:
        selected = (
            address_register["urbanicity"].to_numpy()
            == urbanicity
        )
        n_selected = int(selected.sum())

        if n_selected == 0:
            continue

        probabilities_by_type = type_probabilities[
            urbanicity
        ]
        probabilities = np.array(
            [
                probabilities_by_type[address_type]
                for address_type in ADDRESS_TYPES
            ],
            dtype=float,
        )

        address_types[selected] = rng.choice(
            ADDRESS_TYPES,
            size=n_selected,
            replace=True,
            p=probabilities,
        )

    address_register["address_type"] = address_types

    return address_register[ADDRESS_COLUMNS]


def build_address_sampling_weights(
    address_register: pd.DataFrame,
    config: Mapping[str, Any],
) -> np.ndarray:
    """Return household-sampling weights for synthetic addresses."""

    address_config = config.get("addresses")

    if not isinstance(address_config, Mapping):
        raise ValueError("Configuration must contain an addresses section.")

    weight_config = address_config.get(
        "household_sampling_weights"
    )

    if not isinstance(weight_config, Mapping):
        raise ValueError(
            "addresses must define household_sampling_weights."
        )

    if set(weight_config) != set(ADDRESS_TYPES):
        raise ValueError(
            "Household sampling weights must contain exactly "
            "the configured address types."
        )

    weights_by_type = {
        address_type: float(
            weight_config[address_type]
        )
        for address_type in ADDRESS_TYPES
    }

    if any(
        weight <= 0
        for weight in weights_by_type.values()
    ):
        raise ValueError(
            "Household sampling weights must be positive."
        )

    unknown_types = set(
        address_register["address_type"].dropna()
    ) - set(ADDRESS_TYPES)

    if unknown_types:
        raise ValueError(
            "Address register contains unknown address types."
        )

    if address_register["address_type"].isna().any():
        raise ValueError(
            "Address register contains missing address types."
        )

    return (
        address_register["address_type"]
        .map(weights_by_type)
        .to_numpy(dtype=float)
    )
